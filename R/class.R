# Util for NULL-Handling
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @rdname rn_setup
rebalancedNoise <- R6Class(
  "rebalancedNoise_Class",
  public = list(
    #' @field data_internal Internal data.table including strID mapping and microdata
    data_internal = NULL,
    #' @field dimList Hierarchical definitions (sdcTable format)
    dimList = NULL,
    #' @field numVars Names of the numerical target variables
    numVars = NULL,
    #' @field sensitive_params Parameters for SDC rules (e.g., n_threshold, p_rule)
    sensitive_params = NULL,

    #' @description
    #' Initialize the SDC engine and perform structural mapping.
    #' @param data Input microdata containing `direction` and `noise_multiplier` columns.
    #' @param dimList Hierarchy definitions as used by `sdcTable`.
    #' @param numVars Character vector of numerical variables to perturb.
    #' @param n_threads Integer specifying threads for parallel sensitivity checking and rebalancing.
    #'   Supports `options(rn_threads = X)` or `Sys.setenv(rn_threads = X)`.
    #' @param sensitive_params List of SDC rules (e.g., `list(n_threshold = 3, p_rule = 10)`).
    #' @param ... Additional arguments passed to internal setup.
    initialize = function(data, dimList, numVars, sensitive_params = list(), n_threads = NULL, ...) {
      private$log_info("Initialization started...")

      # Retrieve n_threads from multiple sources
      resolved_threads <- n_threads %||%
        getOption("rn_threads") %||%
        Sys.getenv("rn_threads")

      # if still NULL/empty -> fallback to If it's still NULL or empty string, fallback to parallel::detectCores()
      if (is.null(resolved_threads) || resolved_threads == "") {
        resolved_threads <- max(1, parallel::detectCores() - 1)
      }

      # validate n_threads
      resolved_threads <- suppressWarnings(as.integer(resolved_threads))
      if (is.na(resolved_threads) || resolved_threads < 1) {
        cli::cli_warn("{.arg n_threads} must be a positive integer. Falling back to {.val 1}.")
        resolved_threads <- 1
      }
      private$n_threads <- resolved_threads

      self$numVars <- numVars
      self$dimList <- dimList
      self$sensitive_params <- sensitive_params
      dt <- as.data.table(copy(data))
      dim_names <- names(dimList)

      # Perform structural mapping using sdcTable
      private$log_info("Creating structural mapping via {.pkg sdcTable}...")
      private$prob_object <- sdcTable::makeProblem(
        data = dt,
        dimList = dimList,
        numVarInd = numVars
      )

      # Extract the table skeleton
      private$data_summary <- as.data.table(sdcProb2df(private$prob_object, addDups = TRUE, addNumVars = TRUE, dimCodes = "original"))
      setnames(private$data_summary, "freq", "n_obs")

      struct_mapping <- private$data_summary[, .SD, .SDcols = c(dim_names, "strID")]

      # Identify base cells (leaf nodes) across all dimensions
      private$log_info("Identifying base cells (leaf nodes)...")
      min_info <- lapply(private$prob_object@dimInfo@dimInfo, function(x) {
        data.table(code = slot(x, "codesOriginal"), is_minimal = slot(x, "codesMinimal"))
      })

      is_base_dt <- copy(struct_mapping)
      for (d in dim_names) {
        is_base_dt <- merge(is_base_dt, min_info[[d]], by.x = d, by.y = "code", all.x = TRUE)
        setnames(is_base_dt, "is_minimal", paste0("is_min_", d))
      }

      # A cell is a base cell if it is minimal in ALL dimensions
      min_cols <- paste0("is_min_", dim_names)
      is_base_dt[, is_base_cell := rowSums(.SD == TRUE) == length(dim_names), .SDcols = min_cols]
      private$base_cell_ids <- is_base_dt[is_base_cell == TRUE, as.character(strID)]

      # Join mapping back to microdata
      dt <- merge(dt, struct_mapping, by = dim_names, all.x = TRUE)
      dt[, strID := as.character(strID)]
      self$data_internal <- dt

      private$log_success("Initialization complete.")
    },

    #' @description
    #' Execute the EZS perturbation for a specific variable.
    #' @param target_var Name of the variable to perturb.
    #' @param force Logical. If TRUE, forces recalculation regardless of cache.
    perturb = function(target_var, force = FALSE) {
      if (!(target_var %in% self$numVars)) {
        cli::cli_abort("Variable {.var {target_var}} is not defined in {.arg numVars}.")
      }

      # Check parameter cache
      curr_params <- self$sensitive_params
      if (!force && !is.null(private$pert_status[[target_var]]) &&
          isTRUE(all.equal(curr_params, private$pert_status[[target_var]]))) {
        private$log_info("Variable {.var {target_var}} already calculated. Skipping.")
        return(invisible(self))
      }

      dt <- self$data_internal
      sens_col <- paste0("is_sens_", target_var)

      # Sensitivity Check: Fast-path for count-only threshold
      is_only_n <- (is.null(curr_params$p_rule) || curr_params$p_rule == 0) &&
        (is.null(curr_params$nk_rule$n) || curr_params$nk_rule$n == 0)

      if (is_only_n) {
        private$log_info("Only {.field n_threshold} active.")
        n_thresh <- as.integer(curr_params$n_threshold %||% 0)
        ids_sens <- private$data_summary[n_obs <= n_thresh, as.character(strID)]
        dt[, (sens_col) := strID %in% ids_sens]
      } else {
        private$log_info("Calculating dominance rules via {.fn check_sensitivity_parallel_cpp}...")
        setorderv(dt, c("strID", target_var), c(1, -1))
        group_starts <- which(!duplicated(dt$strID)) - 1
        dt[, (sens_col) := check_sensitivity_cpp(
          vals = get(target_var), ids = strID, group_starts = group_starts,
          n_threshold = as.integer(curr_params$n_threshold %||% 0),
          p_rule = as.double(curr_params$p_rule %||% 0),
          nk_n = as.integer(curr_params$nk_rule$n %||% 0),
          nk_k = as.double(curr_params$nk_rule$k %||% 0),
          n_threads = as.integer(private$n_threads)
        )]
      }

      # Only base cells trigger rebalancing; aggregates are calculated via summation later
      dt[!(strID %in% private$base_cell_ids), (sens_col) := FALSE]

      # Rebalancing setup
      rebal_func <- private$rebalance
      base_ids <- private$base_cell_ids
      p_name <- paste0(target_var, "_pert")
      i_name <- paste0(target_var, "_pert_init")

      private$log_info("Starting rebalancing for {length(unique(base_ids))} base cells...")

      show_pb <- Sys.getenv("SDC_LOG_LEVEL") != "OFF"
      pb <- if(show_pb) progress_bar$new(format = "    [:bar] :percent", total = length(unique(base_ids))) else NULL

      dt[strID %in% unique(base_ids), c(i_name, p_name) := {
        if(!is.null(pb)) pb$tick()
        is_s <- .SD[[sens_col]][1]
        list(rebal_func(.SD, target_var, TRUE), rebal_func(.SD, target_var, is_s))
      }, by = strID]

      # Aggregate calculation
      private$log_info("Aggregating results through hierarchy via {.pkg sdcTable}...")
      all_target_cols <- c(target_var, i_name, p_name)
      tmp_prob <- makeProblem(data = as.data.frame(dt[strID %in% private$base_cell_ids]),
                              dimList = self$dimList, numVarInd = all_target_cols)

      full_res <- as.data.table(sdcProb2df(tmp_prob, addDups = TRUE, addNumVars = TRUE, dimCodes = "original"))

      # Sensitivity mapping for aggregates
      sens_map <- dt[, .(is_sens = any(get(sens_col))), by = strID]
      full_res <- merge(full_res, sens_map, by = "strID", all.x = TRUE)
      full_res[is.na(is_sens), is_sens := FALSE]

      # Identify relevant columns
      new_cols <- c("strID", i_name, p_name, "is_sens")
      update_data <- full_res[, ..new_cols]

      # we need variable-specific names
      setnames(update_data, "is_sens", paste0("is_sens_", target_var))

      # Join everything together
      private$data_summary[update_data, on = "strID",
        (names(update_data)) := mget(paste0("i.", names(update_data)))]

      private$pert_status[[target_var]] <- copy(curr_params)
      private$log_success("Perturbation for {.var {target_var}} complete.")
    },

    #' Retrieve Aggregated Results
    #' @param target_var Character name of the variable. If NULL, returns all
    #' previously computed results.
    #' @param format Character, either "wide" (default) or "long".
    #' @return A data.table with results and percentage deviations.
    get_results = function(target_var = NULL, format = "wide") {
      if (!format %in% c("wide", "long")) {
        cli::cli_abort("{.arg format} must be either {.val wide} or {.val long}.")
      }

      # Define variables to process
      available_vars <- Filter(function(v) {
        paste0(v, "_pert") %in% names(private$data_summary)
      }, self$numVars)

      vars_to_process <- target_var %||% available_vars

      if (length(vars_to_process) == 0) {
        cli::cli_abort("No results found. Call {.fn perturb} first!")
      }

      # Extract results into long-format
      extract_long = function(v) {
        calc_pct = function(p, o) {
          round(data.table::fifelse(o == 0, 0, (p - o) / o * 100), digits = 3)
        }
        col_p_init  <- paste0(v, "_pert_init")
        col_p_final <- paste0(v, "_pert")
        col_is_sens <- paste0("is_sens_", v)

        dim_names <- names(self$dimList)
        # Ensure n_obs and strID are included for metadata/mapping
        meta_cols <- c(dim_names, "n_obs", "strID")
        required_cols <- c(meta_cols, v, col_p_init, col_p_final, col_is_sens)

        # Subsetting master table
        res_dt <- data.table::copy(private$data_summary[, ..required_cols])

        # Add flags
        res_dt[, is_internal := (strID %in% private$base_cell_ids)]
        res_dt[, strID := NULL]

        # Standardize names to long format
        data.table::setnames(res_dt,
                             old = c(v, col_p_init, col_p_final, col_is_sens),
                             new = c("val_orig", "val_pert_init", "val_pert", "is_sens"))

        # Calculations
        res_dt[, diff_init_pct := calc_pct(val_pert_init, val_orig)]
        res_dt[, diff_final_pct := calc_pct(val_pert, val_orig)]
        res_dt[, variable := v]

        return(res_dt)
      }

      # Combine into one Long table
      res_long <- data.table::rbindlist(lapply(vars_to_process, extract_long))

      # if requested, transform long -> wide
      if (format == "long") {
        return(res_long)
      } else {
        # Identify non-value columns for the LHS of the formula
        dim_cols <- names(self$dimList)
        id_cols  <- c(dim_cols, "n_obs", "is_internal")

        # Construct Formula
        lhs <- paste(id_cols, collapse = " + ")
        form_str <- paste(lhs, "~ variable")

        # Long -> Wide
        res_wide <- data.table::dcast(
          res_long,
          formula = as.formula(form_str),
          value.var = c("val_orig", "val_pert_init", "val_pert", "is_sens", "diff_init_pct", "diff_final_pct"),
          sep = "_"
        )

        # Cleanup column names
        new_names <- names(res_wide)
        for (v in vars_to_process) {
          new_names <- gsub(paste0("(.*)_", v, "$"), paste0(v, "_\\1"), new_names)
        }
        new_names <- gsub("_val_", "_", new_names)
        data.table::setnames(res_wide, new_names)

        return(res_wide)
      }
    },

    #' @description
    #' Summarize perturbation impact and rebalancing efficiency.
    #' @param target_var Character name of the variable to summarize.
    #' @return Invisibly returns a list of summary statistics.
    summarize = function(target_var) {
      p_name <- paste0(target_var, "_pert")
      if (!p_name %in% names(private$data_summary)) {
        cli::cli_abort("Variable {.var {target_var}} has not been perturbed yet.")
      }

      res <- self$get_results(target_var, format = "long")
      probs <- c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)

      calc_group <- function(dt) {
        if (nrow(dt) == 0) return(NULL)
        list(
          count = nrow(dt),
          mape_init  = mean(abs(dt$diff_init_pct), na.rm = TRUE),
          mape_final = mean(abs(dt$diff_final_pct), na.rm = TRUE),

          # Relative Distributions
          q_rel_init  = quantile(dt$diff_init_pct, probs = probs, na.rm = TRUE),
          q_rel_final = quantile(dt$diff_final_pct, probs = probs, na.rm = TRUE),

          # Absolute Distribution (Final)
          q_abs_final = quantile(dt$val_pert - dt$val_orig, probs = probs, na.rm = TRUE)
        )
      }

      render_q_row <- function(q_vec) {
        # Format: Min | 1% | 5% | 25% | 50% | 75% | 95% | 99% | Max
        p <- round(q_vec, 3)
        cli::cli_text("  {p[1]} | {p[2]} | {p[3]} | {p[4]} | {.strong {p[5]}} | {p[6]} | {p[7]} | {p[8]} | {p[9]}")
      }

      g_all  <- calc_group(res)
      g_nons <- calc_group(res[is_sens == FALSE])
      g_sens <- calc_group(res[is_sens == TRUE])

      cli::cli_h1("EZS Perturbation Summary: {.val {target_var}}")

      groups <- list(
        list(val = g_all,  title = "OVERALL (All Cells)", compare = TRUE),
        list(val = g_nons, title = "NON-SENSITIVE (Rebalanced)", compare = TRUE),
        list(val = g_sens, title = "SENSITIVE (Fixed Noise)", compare = FALSE)
      )

      for (g in groups) {
        v <- g$val
        if (is.null(v)) next
        cli::cli_h2(g$title)

        # Metrics
        line1 <- paste0("nrCells: ", v$count, " | MAPE: ", round(v$mape_final, 3),
                        "% (Initial: ", round(v$mape_init, 3), "%)")
        if (g$compare) {
          red <- (1 - (v$mape_final / v$mape_init)) * 100
          line1 <- paste0(line1, " | Noise Reduction: ", round(red, 1), "%")
        }
        cli::cli_alert_info(line1)
        cli::cli_text("{.strong Percentiles (Min | 1% | 5% | 25% | 50% | 75% | 95% | 99% | Max):}")

        if (g$compare) {
          cli::cli_text("{.field Relative (%) - Initial (Balanced):}"); render_q_row(v$q_rel_init)
          cli::cli_text("{.field Relative (%) - Final (Rebalanced):}"); render_q_row(v$q_rel_final)
        } else {
          cli::cli_text("{.field Relative (%) - Final:}"); render_q_row(v$q_rel_final)
        }

        cli::cli_text("{.field Absolute (Units) - Final:}"); render_q_row(v$q_abs_final)
      }

      return(invisible(list(all = g_all, non_sensitive = g_nons, sensitive = g_sens)))
    }
  ),

  private = list(
    n_threads = 1,
    data_summary = list(),
    pert_status = list(),
    base_cell_ids = NULL,
    prob_object = NULL,

    log_info = function(msg) {
      if (Sys.getenv("SDC_LOG_LEVEL") != "OFF") {
        # parent.frame() erlaubt cli, Variablen aus der aufrufenden Funktion zu sehen
        cli::cli_alert_info(msg, .envir = parent.frame())
      }
    },
    log_success = function(msg) {
      if (Sys.getenv("SDC_LOG_LEVEL") != "OFF") {
        cli::cli_alert_success(msg, .envir = parent.frame())
      }
    },

    # Core EZS Rebalancing logic
    rebalance = function(sub_dt, v, is_sens_override) {
      n_obs <- nrow(sub_dt)
      v_dt <- sub_dt[, .(orig = .SD[[v]], mult = noise_multiplier, dirs = direction)]
      v_dt[, original_idx := .I]
      v_dt[, impact := abs(orig * mult)]
      setorder(v_dt, -impact) # Sort descending by impact (abs(orig * mult))

      orig <- v_dt$orig; mult <- v_dt$mult; dirs <- v_dt$dirs
      pert <- numeric(n_obs)

      # First record determines initial direction
      pert[1] <- orig[1] * (1 + (dirs[1] * mult[1]))
      running_noise <- pert[1] - orig[1]

      if (n_obs > 1) {
        for (i in 2:n_obs) {
          # If sensitive: use fixed direction. Otherwise: minimize running noise.
          d_opt <- if (!is_sens_override) {
            if (abs(running_noise + (orig[i] * mult[i])) < abs(running_noise - (orig[i] * mult[i]))) 1 else -1
          } else {
            dirs[i]
          }
          pert[i] <- orig[i] * (1 + (d_opt * mult[i]))
          running_noise <- running_noise + (pert[i] - orig[i])
        }
      }
      v_dt$pert_val <- pert
      setorder(v_dt, original_idx)
      return(v_dt$pert_val)
    }
  )
)

