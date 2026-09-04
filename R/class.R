# Util for NULL-Handling
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @rdname rn_setup
rebalancedNoise <- R6Class(
  "rebalancedNoise_Class",
  public = list(
    #' @field sensitive_params Parameters for SDC rules (e.g., n_threshold, p_rule)
    sensitive_params = NULL,

    #' @description
    #' Initialize the SDC engine. No hierarchy definition needed at setup.
    #' @param data Input microdata containing `direction` and `noise_multiplier` columns.
    #' @param n_threads Integer specifying threads for parallel sensitivity checking and rebalancing.
    #'   Supports `options(rn_threads = X)` or `Sys.setenv(rn_threads = X)`.
    #' @param sensitive_params List of SDC rules (e.g., `list(n_threshold = 3, p_rule = 10)`).
    #' @param import_rebal_status Internal parameter for restoring rebalancing status from export.
    #' @param import_result_tables Internal parameter for restoring result tables from export.
    #' @param ... Additional arguments passed to internal setup.
    initialize = function(
      data,
      sensitive_params = list(),
      n_threads = NULL,
      import_rebal_status = NULL,
      import_result_tables = NULL,
      ...
    ) {
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
        cli::cli_warn(
          "{.arg n_threads} must be a positive integer. Falling back to {.val 1}."
        )
        resolved_threads <- 1
      }
      private$n_threads <- resolved_threads

      self$sensitive_params <- sensitive_params
      dt <- as.data.table(copy(data))

      # Add internal record ID for tracking
      dt[, record_id := .I]

      # Store microdata with original direction, rebalanced direction, and noise_multiplier
      private$microdata <- dt

      # Initialize status tracking
      if (!is.null(import_rebal_status)) {
        # Restore from export
        private$rebal_status <- import_rebal_status
      } else {
        # Fresh initialization
        private$rebal_status <- list(
          done = FALSE,
          dim_list = NULL,
          params = NULL
        )
      }

      # Initialize result tables
      if (!is.null(import_result_tables)) {
        private$result_tables <- import_result_tables
      } else {
        private$result_tables <- list()
      }

      private$log_success("Initialization complete.")
    },

    #' @description
    #' Perform rebalancing ONCE and store updated direction values in microdata.
    #' @param dim_list Named list of hierarchies defining the detailed table structure
    #'   for rebalancing (e.g., `list(nace = hier_detailed, bgkl = hier_detailed)`).
    #' @param num_var Name of the numerical variable to rebalance.
    #'   Currently only a single variable is supported.
    #' @return Invisibly returns the object (for chaining).
    rebalance = function(dim_list, num_var) {
      # Validate dim_list
      if (!is.list(dim_list) || is.null(names(dim_list))) {
        cli::cli_abort(
          "{.arg dim_list} must be a {.strong named} list of hierarchies."
        )
      }

      # Validate num_var
      if (!is.character(num_var) || length(num_var) != 1) {
        cli::cli_abort("{.arg num_var} must be a single character string.")
      }

      # Check hierarchy names match data
      missing_dims <- setdiff(names(dim_list), names(private$microdata))
      if (length(missing_dims) > 0) {
        cli::cli_abort(
          "Dimension {.val {missing_dims}} in {.arg dim_list} not found in data."
        )
      }

      # Check num_var exists in data
      if (!(num_var %in% names(private$microdata))) {
        cli::cli_abort(
          "Variable {.val {num_var}} not found in data."
        )
      }

      target_var <- num_var

      private$log_info("Performing rebalancing on detailed table structure...")

      # Call helper function to perform full rebalancing
      updated_microdata <- .perform_ezs_rebalancing(
        data = private$microdata,
        dimList = dim_list,
        numVars = target_var,
        sensitive_params = self$sensitive_params,
        n_threads = private$n_threads
      )

      # Update microdata with rebalanced directions
      private$microdata <- updated_microdata

      # Update rebalancing status
      private$rebal_status <- list(
        done = TRUE,
        dim_list = dim_list,
        params = self$sensitive_params,
        num_var = target_var
      )

      private$log_success(
        "Rebalancing complete. Updated {.val direction_rebalanced} column."
      )
      return(invisible(self))
    },

    #' @description
    #' Execute the EZS perturbation for specific variables using stored directions.
    #' @param dim_list Named list of hierarchies defining the table structure
    #'   for this perturbation.
    #' @param variables Character vector of variable name(s) to perturb.
    #' @param name Character name for this result table (stored in results list).
    #' @param force Logical. If TRUE, forces recalculation regardless of cache.
    #' @param round Logical. If TRUE, the perturbed values are rounded to
    #'   whole numbers with `round()` before the tables are computed. This
    #'   makes all published cell values integers. Rounding can add up to 0.5
    #'   per record, so cell totals may shift a little. Default is `FALSE`.
    #' @return Invisibly returns the object (for chaining).
    perturb = function(
      dim_list,
      variables,
      name,
      force = FALSE,
      round = FALSE
    ) {
      # Check if rebalancing was done
      if (!private$rebal_status$done) {
        cli::cli_abort(c(
          "x" = "Rebalancing has not been performed yet.",
          "i" = "Call {.fn rebalance} before {.fn perturb}."
        ))
      }

      # Validate variables
      if (!is.character(variables) || length(variables) == 0) {
        cli::cli_abort(
          "{.arg variables} must be a non-empty character vector."
        )
      }

      # Check that all target variables exist in data
      for (tv in variables) {
        if (!(tv %in% names(private$microdata))) {
          cli::cli_abort(
            "Variable {.var {tv}} not found in data."
          )
        }
      }

      # Validate name parameter
      if (is.null(name) || !is.character(name) || length(name) != 1) {
        cli::cli_abort("{.arg name} must be a single character string.")
      }

      # Validate round parameter
      if (!is.logical(round) || length(round) != 1 || is.na(round)) {
        cli::cli_abort("{.arg round} must be a single logical value.")
      }

      # Generate table hash from dim_list, sensitive_params and round flag
      table_hash <- .get_table_hash(dim_list, self$sensitive_params, round)

      # Check if table name already exists with different hash
      if (name %in% names(private$result_tables)) {
        existing_hash <- private$result_tables[[name]]$hash
        if (existing_hash != table_hash) {
          cli::cli_abort(c(
            "x" = "Table name {.val {name}} is already in use.",
            "i" = "Use a different table name for this hierarchy definition."
          ))
        }
      }

      # Loop over all target variables
      for (tv in variables) {
        # Check cache for this specific variable
        cache_key <- paste(name, tv, collapse = "_")
        curr_params <- self$sensitive_params
        curr_cache <- list(params = curr_params, round = round)
        if (
          !force &&
            !is.null(private$pert_status[[cache_key]]) &&
            isTRUE(all.equal(curr_cache, private$pert_status[[cache_key]]))
        ) {
          private$log_info(
            "Table {.val {name}} for {.var {tv}} already calculated. Skipping."
          )
          next
        }

        # Start with microdata
        dt <- copy(private$microdata)

        # Compute initial perturbation (using original random directions)
        init_name <- paste0(tv, "_init")
        dt[, (init_name) := get(tv) * (1 + direction * noise_multiplier)]

        # Compute final perturbation (using rebalanced directions)
        p_name <- paste0(tv, "_pert")
        dt[,
          (p_name) := get(tv) * (1 + direction_rebalanced * noise_multiplier)
        ]

        # Optionally round perturbed values so tabulated cells are integers
        if (round) {
          dt[, (init_name) := round(get(init_name))]
          dt[, (p_name) := round(get(p_name))]
        }

        # Aggregate original, initial, and final through hierarchy
        prob_object <- sdcTable::makeProblem(
          data = dt,
          dimList = dim_list,
          numVarInd = c(tv, init_name, p_name)
        )

        full_res <- as.data.table(
          sdcProb2df(
            prob_object,
            addDups = TRUE,
            addNumVars = TRUE,
            dimCodes = "original"
          )
        )
        full_res$sdcStatus <- NULL
        setnames(full_res, "freq", "n_obs")

        # Re-join microdata with new strID from this perturb call's dim_list
        dim_names <- names(dim_list)
        # Deduplicate dimension combinations to avoid Cartesian product with bogus codes
        struct_mapping <- unique(full_res[, .SD, .SDcols = c(dim_names, "strID")], by = dim_names)
        dt <- merge(
          dt,
          struct_mapping,
          by = dim_names,
          all.x = TRUE,
          suffixes = c("", ".new")
        )
        if ("strID.new" %in% names(dt)) {
          dt[, strID := as.character(strID.new)]
          dt[, strID.new := NULL]
        }

        # Compute sensitivity for this result table
        sens_result <- .compute_cell_sensitivity(
          microdata = dt,
          sensitive_params = curr_params,
          target_var = tv,
          n_threads = private$n_threads
        )

        # Remove n_obs from sens_result to avoid duplicate columns
        sens_result[, n_obs := NULL]

        # Identify base cells (only base cells can be sensitive)
        base_cells <- .identify_base_cells(
          prob_object,
          names(dim_list),
          full_res
        )

        # Merge sensitivity into full_res
        full_res <- merge(full_res, sens_result, by = "strID", all.x = TRUE)
        sens_col_name <- paste0("is_sens_", tv)
        full_res[is.na(get(sens_col_name)), (sens_col_name) := FALSE]

        # Only base cells are sensitive; aggregates are FALSE
        # Deduplicate base_cells by strID to avoid Cartesian product when all hierarchy
        # levels share the same strID
        if (nrow(base_cells) > 0) {
          base_cells <- base_cells[, .SD[1], by = "strID"]
        }
        full_res <- merge(full_res, base_cells, by = "strID", all.x = TRUE)
        full_res[is.na(is_base_cell), is_base_cell := FALSE]
        full_res[is_base_cell == FALSE, (sens_col_name) := FALSE]

        # Rename is_base_cell to is_internal for user-facing output
        setnames(full_res, "is_base_cell", "is_internal")

        # Deduplicate by dimension columns to remove hierarchy-level duplicates
        # sdcProb2df with addDups=TRUE creates multiple rows for the same dimension
        # value when it appears at different hierarchy levels (e.g., b, b01, b001)
        # All these rows share the same strID, so we keep one row per dimension combo
        if (nrow(full_res) > 0) {
          full_res <- full_res[, .SD[1], by = names(dim_list)]
        }

        # Keep strID for sorting in get_results()

        # Reorder columns for new tables: dims + meta + grouped by variable
        all_cols <- names(full_res)
        all_vars <- unique(gsub(
          "_init$|_pert$|_is_sens$",
          "",
          grep("_init$|_pert$|_is_sens$", all_cols, value = TRUE)
        ))

        # Build ordered column list
        ordered_cols <- c(names(dim_list), "n_obs", "is_internal")
        for (v in all_vars) {
          v_cols <- grep(
            paste0("^", v, "(_init|_pert|_is_sens)?$"),
            all_cols,
            value = TRUE
          )
          ordered_cols <- c(ordered_cols, v_cols)
        }

        # Only reorder if we have all columns
        if (all(ordered_cols %in% all_cols)) {
          setcolorder(full_res, ordered_cols)
        }

        # Create result entry structure (only for first variable)
        if (tv == variables[1]) {
          result_entry <- list(
            hash = table_hash,
            dim_list = dim_list,
            sensitive_params = curr_params,
            round = round,
            base_cells = base_cells[, .(strID, is_base_cell)],
            table = full_res,
            variables = list()
          )
          result_entry$variables[[tv]] <- list(
            sensitive_params = curr_params,
            is_sens_col = sens_col_name
          )
        } else {
          # Merge new columns into existing table
          existing_table <- private$result_tables[[name]]$table
          merge_cols <- names(dim_list)

          # Get only the new columns we need to add (including original variable)
          cols_to_add <- c(tv, init_name, p_name, sens_col_name)

          # Deduplicate dimension combinations to avoid Cartesian product with bogus codes
          full_res_subset <- unique(full_res[, .SD, .SDcols = c(merge_cols, cols_to_add)], by = merge_cols)

          # Merge, keeping all existing columns and adding new ones
          private$result_tables[[name]]$table <- merge(
            existing_table,
            full_res_subset,
            by = merge_cols,
            all.x = TRUE
          )

          # Remove duplicate is_internal if it exists (keep the original)
          if ("is_internal.y" %in% names(private$result_tables[[name]]$table)) {
            private$result_tables[[name]]$table[, is_internal.y := NULL]
          }

          # Add variable to metadata
          private$result_tables[[name]]$variables[[tv]] <- list(
            sensitive_params = curr_params,
            is_sens_col = sens_col_name
          )

          private$log_success(
            "Added {.var {tv}} to existing table {.val {name}}."
          )

          # Update cache
          private$pert_status[[cache_key]] <- copy(curr_cache)
          next
        }

        if (name %in% names(private$result_tables)) {
          # Merge new columns into existing table (for first variable in multi-var call)
          existing_table <- private$result_tables[[name]]$table
          merge_cols <- names(dim_list)

          # Get only the new columns we need to add (including original variable)
          cols_to_add <- c(tv, init_name, p_name, sens_col_name)

          # Deduplicate dimension combinations to avoid Cartesian product with bogus codes
          full_res_subset <- unique(full_res[, .SD, .SDcols = c(merge_cols, cols_to_add)], by = merge_cols)

          # Merge, keeping all existing columns and adding new ones
          private$result_tables[[name]]$table <- merge(
            existing_table,
            full_res_subset,
            by = merge_cols,
            all.x = TRUE
          )

          # Remove duplicate is_internal if it exists (keep the original)
          if ("is_internal.y" %in% names(private$result_tables[[name]]$table)) {
            private$result_tables[[name]]$table[, is_internal.y := NULL]
          }

          # Reorder columns: dims + meta + grouped by variable
          all_cols <- names(private$result_tables[[name]]$table)
          all_vars <- unique(gsub(
            "_init$|_pert$|_is_sens$",
            "",
            grep("_init$|_pert$|_is_sens$", all_cols, value = TRUE)
          ))

          # Build ordered column list
          ordered_cols <- c(names(dim_list), "n_obs", "is_internal")
          for (v in all_vars) {
            v_cols <- grep(
              paste0("^", v, "(_init|_pert|_is_sens)?$"),
              all_cols,
              value = TRUE
            )
            ordered_cols <- c(ordered_cols, v_cols)
          }

          # Only reorder if we have all columns
          if (all(ordered_cols %in% all_cols)) {
            setcolorder(private$result_tables[[name]]$table, ordered_cols)
          }

          # Add variable to metadata
          private$result_tables[[name]]$variables[[tv]] <- list(
            sensitive_params = curr_params,
            is_sens_col = sens_col_name
          )

          private$log_success(
            "Added {.var {tv}} to existing table {.val {name}}."
          )
        } else {
          # New table - store entry
          private$result_tables[[name]] <- result_entry

          private$log_success(
            "Created new table {.val {name}} with variable {.var {tv}}."
          )
        }

        # Update cache
        private$pert_status[[cache_key]] <- copy(curr_cache)
      }

      return(invisible(self))
    },

    #' @description
    #' Retrieve aggregated results from perturbation calls.
    #' @param name Character name of the result table to retrieve. If NULL, returns all tables.
    #' @param format Character, either "wide" (default) or "long".
    #' @param variables Character vector of variable names to include. If NULL, includes all variables.
    #' @return A data.table (single table) or named list of data.tables (all tables).
    get_results = function(name = NULL, format = "wide", variables = NULL) {
      if (!format %in% c("wide", "long")) {
        cli::cli_abort(
          "{.arg format} must be either {.val wide} or {.val long}."
        )
      }

      # Check if any results exist
      if (length(private$result_tables) == 0) {
        cli::cli_abort(
          "No results found. Call {.fn perturb} first!"
        )
      }

      # Helper function to process a single table
      process_table <- function(table, stored_vars) {
        # Determine which variables to include
        if (is.null(variables)) {
          target_vars <- names(stored_vars)
        } else {
          # Validate requested variables
          missing <- setdiff(variables, names(stored_vars))
          if (length(missing) > 0) {
            available <- paste(names(stored_vars), collapse = ", ")
            cli::cli_abort(c(
              "x" = "Variable(s) {.val {missing}} not found in table.",
              "i" = "Available variables: {.val {available}}"
            ))
          }
          target_vars <- variables
        }

        if (format == "long") {
          all_cols <- names(table)

          # Filter to only requested variables
          target_vars_full <- target_vars
          target_vars <- target_vars_full[target_vars_full %in% all_cols]

          if (length(target_vars) > 0) {
            # Get all value columns for requested variables
            value_cols <- unlist(lapply(target_vars, function(v) {
              c(v, paste0(v, "_init"), paste0(v, "_pert"))
            }))
            value_cols <- intersect(value_cols, all_cols)

            # Get is_sens columns for requested variables
            sens_cols <- paste0("is_sens_", target_vars_full)
            sens_cols <- intersect(sens_cols, all_cols)

            # Get all is_sens columns
            all_sens_cols <- grep("^is_sens_", all_cols, value = TRUE)

            # Columns to remove: non-requested value columns + non-requested is_sens columns
            all_vars_in_table <- unique(gsub("_init$|_pert$", "", 
                              grep("_init$|_pert$", all_cols, value = TRUE)))
            all_value_cols <- unlist(lapply(all_vars_in_table, function(v) {
              c(v, paste0(v, "_init"), paste0(v, "_pert"))
            }))
            all_value_cols <- intersect(all_value_cols, all_cols)

            value_to_remove <- setdiff(all_value_cols, value_cols)
            sens_to_remove <- setdiff(all_sens_cols, sens_cols)
            cols_to_remove <- c(value_to_remove, sens_to_remove)

            # Get id columns (dimension columns + metadata + is_sens for requested vars)
            id_cols <- setdiff(all_cols, c(all_value_cols, sens_to_remove))

            # Melt all value columns
            res_long <- data.table::melt(
              table,
              id.vars = id_cols,
              measure.vars = value_cols,
              variable.name = "temp_var",
              value.name = "value"
            )

            # Extract variable name and measure type
            res_long[, var_name := gsub("_init$|_pert$", "", temp_var)]
            res_long[, measure_type := ifelse(grepl("_init$", temp_var), "init",
                                       ifelse(grepl("_pert$", temp_var), "pert", "orig"))]

            # Cast to wide format per variable
            res_long[, temp_var := NULL]
            res_long <- data.table::dcast(
              res_long,
              ... ~ measure_type,
              value.var = "value"
            )
            setnames(res_long, c("orig", "init", "pert"),
                     c("val_orig", "val_pert_init", "val_pert"))

            # Add is_sensitive column by looking up the correct is_sens column
            res_long[, is_sensitive := NA]
            for (v in target_vars) {
              sens_col <- paste0("is_sens_", v)
              if (sens_col %in% names(res_long)) {
                res_long[var_name == v, is_sensitive := get(sens_col)]
                res_long[, (sens_col) := NULL]
              }
            }

            # Rename var_name to variable
            setnames(res_long, "var_name", "variable")

            # Calculate percentage differences
            res_long[,
              diff_init_pct := round(
                (val_pert_init - val_orig) / val_orig * 100,
                digits = 3
              )
            ]
            res_long[,
              diff_final_pct := round(
                (val_pert - val_orig) / val_orig * 100,
                digits = 3
              )
            ]

            # Clean up: remove strID if present
            if ("strID" %in% names(res_long)) {
              setorder(res_long, strID)
              res_long[, strID := NULL]
            }

            # Set variable column name
            # (the var_name column was already removed, need to recreate from the melt)

            return(res_long)
          }
        }

        # Wide format: remove columns for non-requested variables
        if (length(target_vars) < length(names(stored_vars))) {
          # Get all columns related to variables to remove
          vars_to_remove <- setdiff(names(stored_vars), target_vars)
          if (length(vars_to_remove) > 0) {
            cols_to_remove <- unlist(lapply(vars_to_remove, function(v) {
              c(v, paste0(v, "_init"), paste0(v, "_pert"), paste0("is_sens_", v))
            }))
            cols_to_remove <- intersect(cols_to_remove, names(table))
            if (length(cols_to_remove) > 0) {
              table[, (cols_to_remove) := NULL]
            }
          }
        }

        # Remove strID if present
        if ("strID" %in% names(table)) {
          setorder(table, strID)
          table[, strID := NULL]
        }

        return(table)
      }

      # If name is NULL, return all results as named list
      if (is.null(name)) {
        result_list <- lapply(private$result_tables, function(res) {
          process_table(res$table, res$variables)
        })
        return(result_list)
      }

      # If name is provided, return specific result
      if (!(name %in% names(private$result_tables))) {
        available_names <- paste(names(private$result_tables), collapse = ", ")
        cli::cli_abort(c(
          "x" = "Result table {.val {name}} not found.",
          "i" = "Available tables: {.val {available_names}}"
        ))
      }

      stored_result <- private$result_tables[[name]]
      result <- copy(stored_result$table)

      result <- process_table(result, stored_result$variables)
      return(result)
    },

    #' @description
    #' Summarize perturbation impact with 3-way comparison.
    #' @param table Character name of the result table to summarize.
    #' @param target_vars Optional character vector of variable names to summarize.
    #'   If NULL, summarizes all variables in the table.
    #' @return Invisibly returns a named list of summary statistics (one per variable).
    summarize = function(table, target_vars = NULL) {
      # Validate table name
      if (!(table %in% names(private$result_tables))) {
        available <- paste(names(private$result_tables), collapse = ", ")
        cli::cli_abort(c(
          "x" = "Result table {.val {table}} not found.",
          "i" = "Available tables: {.val {available}}"
        ))
      }

      res <- private$result_tables[[table]]$table

      # Determine which variables to summarize
      if (is.null(target_vars)) {
        target_vars_all <- names(private$result_tables[[table]]$variables)
      } else {
        target_vars_all <- target_vars
      }

      # Validate target variables exist
      missing <- setdiff(
        target_vars_all,
        names(private$result_tables[[table]]$variables)
      )
      if (length(missing) > 0) {
        cli::cli_abort(c(
          "x" = "Variable(s) {.val {missing}} not found in table {.val {table}}.",
          "i" = "Available variables: {.val {names(private$result_tables[[table]]$variables)}}"
        ))
      }

      # Summary for each variable
      result_list <- list()

      for (v in target_vars_all) {
        sens_col <- paste0("is_sens_", v)
        if (!(sens_col %in% names(res))) {
          cli::cli_abort("Sensitivity column {.val {sens_col}} not found.")
        }

        # Create unified data table
        res_temp <- copy(res)

        # Define columns
        orig_col <- v
        init_col <- paste0(v, "_init")
        pert_col <- paste0(v, "_pert")

        # Compute deviations
        res_temp[,
          diff_init_pct := round(
            (get(init_col) - get(orig_col)) / get(orig_col) * 100,
            digits = 3
          )
        ]
        res_temp[,
          diff_final_pct := round(
            (get(pert_col) - get(orig_col)) / get(orig_col) * 100,
            digits = 3
          )
        ]

        probs <- c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)

        # Compute statistics for all cells
        count <- nrow(res_temp)
        mape_init <- mean(abs(res_temp$diff_init_pct), na.rm = TRUE)
        mape_final <- mean(abs(res_temp$diff_final_pct), na.rm = TRUE)
        q_rel_init <- quantile(res_temp$diff_init_pct, probs = probs, na.rm = TRUE)
        q_rel_final <- quantile(res_temp$diff_final_pct, probs = probs, na.rm = TRUE)
        q_abs_final <- quantile(res_temp[[pert_col]] - res_temp[[orig_col]], probs = probs, na.rm = TRUE)

        # Noise reduction (overall comparison of init vs final)
        noise_red <- if (mape_init > 0) {
          round((1 - mape_final / mape_init) * 100, 2)
        } else {
          NA_real_
        }

        # Compute metadata
        n_total <- count
        n_sens <- sum(res_temp[[sens_col]], na.rm = TRUE)
        n_internal <- sum(res_temp$is_internal)

        # Helper functions for display
        render_percentiles <- function(q_vec, label = NULL) {
          p <- round(q_vec, 3)
          if (!is.null(label)) {
            cli::cli_text("{.strong {label}}:")
          }
          cli::cli_text(
            "  {p[1]} | {p[2]} | {p[3]} | {p[4]} | {.strong {p[5]}} | {p[6]} | {p[7]} | {p[8]} | {p[9]}"
          )
        }

        render_absolute <- function(q_vec, label = NULL) {
          p <- round(q_vec, 3)
          if (!is.null(label)) {
            cli::cli_text("{.strong {label}}:")
          }
          cli::cli_text(
            "  {p[1]} | {p[2]} | {p[3]} | {p[4]} | {.strong {p[5]}} | {p[6]} | {p[7]} | {p[8]} | {p[9]}"
          )
        }

        # Display output
        cli::cli_h1("EZS Perturbation Summary: {.val {table}}")
        cli::cli_h2("Variable: {.val {v}}")

        # OVERALL group (all cells)
        cli::cli_h3("OVERALL")
        cli::cli_alert_info(
          paste0(
            "nrCells: ",
            count,
            " | MAPE (Initial): ",
            round(mape_init, 3),
            "% | MAPE (Final): ",
            round(mape_final, 3),
            "%",
            if (!is.na(noise_red)) paste0(" | Noise Reduction: ", noise_red, "%") else ""
          )
        )
        render_percentiles(q_rel_init, "Percentiles - Relative (%) Initial")
        render_percentiles(q_rel_final, "Percentiles - Relative (%) Final")
        render_absolute(q_abs_final, "Percentiles - Absolute (Units) Final")

        # Summary metadata
        cli::cli_h3("Summary Statistics")
        cli::cli_ul(list(
          paste0(
            "Sensitivity Rate: ",
            round(n_sens / n_total * 100, 2),
            "% (",
            n_sens,
            "/",
            n_total,
            " cells)"
          ),
          paste0(
            "Internal Cells: ",
            n_internal,
            " (",
            round(n_internal / n_total * 100, 1),
            "%)"
          ),
          if (!is.na(noise_red)) {
            paste0("Noise Reduction: ", noise_red, "%")
          } else {
            "Noise Reduction: N/A"
          }
        ))

        # Store in result list
        result_list[[v]] <- list(
          overall = list(
            count = count,
            mape_init = mape_init,
            mape_final = mape_final,
            q_rel_init = q_rel_init,
            q_rel_final = q_rel_final,
            q_abs_final = q_abs_final
          ),
          meta = list(
            sensitivity_rate = round(n_sens / n_total * 100, 2),
            n_total_cells = n_total,
            n_sensitive_cells = n_sens,
            n_internal_cells = n_internal,
            noise_reduction_pct = noise_red
          )
        )
      }

      # If single variable, return that element; otherwise return list
      if (length(target_vars_all) == 1 && !is.null(target_vars)) {
        return(invisible(result_list[[target_vars_all]]))
      }

      return(invisible(result_list))
    },

    #' @description
    #' Export rebalanced microdata and optional results to a reusable format.
    #' @param include_results Logical. If TRUE, includes cached perturbation results.
    #' @param file Character path to save RDS file. If NULL, returns export object in memory.
    #' @return If file is NULL, returns the export object. If file is provided, returns
    #'   the object invisibly for chaining.
    export = function(include_results = TRUE, file = NULL) {
      # Build result tables metadata with hashes for validation
      result_meta <- NULL
      if (include_results && length(private$result_tables) > 0) {
        result_meta <- lapply(names(private$result_tables), function(name) {
          list(
            name = name,
            hash = private$result_tables[[name]]$hash,
            dim_list = private$result_tables[[name]]$dim_list,
            sensitive_params = private$result_tables[[name]]$sensitive_params,
            round = private$result_tables[[name]]$round,
            variables = names(private$result_tables[[name]]$variables)
          )
        })
        names(result_meta) <- NULL
      }

      out <- list(
        microdata = copy(private$microdata),
        sensitive_params = self$sensitive_params,
        rebal_status = private$rebal_status,
        result_tables = if (include_results) private$result_tables else NULL,
        result_meta = result_meta,
        export_metadata = list(
          timestamp = Sys.time(),
          package_version = as.character(utils::packageVersion(
            "rebalancedNoise"
          ))
        )
      )
      class(out) <- "rebalancedNoise_ExportData"

      if (!is.null(file)) {
        if (!is.character(file) || length(file) != 1) {
          cli::cli_abort("{.arg file} must be a single character string.")
        }
        saveRDS(out, file = file)
        cli::cli_alert_success("Exported to {.path {file}}")
        return(invisible(self))
      }

      return(out)
    },

    #' @description
    #' Extract current microdata with direction columns.
    #' @param include_record_id Logical. If TRUE, includes the internal `record_id` column
    #'   used for tracking records during rebalancing. Default is FALSE.
    #' @return A `data.table` with microdata including `direction` and `direction_rebalanced`
    #'   columns. Returns a copy to prevent modification of internal data.
    get_microdata = function(include_record_id = FALSE) {
      if (include_record_id) {
        return(copy(private$microdata))
      }
      dt <- copy(private$microdata)
      dt[, record_id := NULL]
      return(dt)
    },

    #' @description
    #' List available perturbed tables with their dimension labels and variables.
    #' @return A `data.table` with columns:
    #'   - `table_name`: Character name of the perturbed table
    #'   - `dimensions`: Dimension hierarchy labels joined with " x "
    #'   - `variables`: Comma-separated list of perturbed variables
    list_tables = function() {
      if (length(private$result_tables) == 0) {
        cli::cli_alert_warning(
          "No perturbed tables available. Call {.fn perturb} first."
        )
        return(data.table::data.table(
          table_name = character(0),
          dimensions = character(0),
          variables = character(0)
        ))
      }

      table_info <- lapply(names(private$result_tables), function(name) {
        res <- private$result_tables[[name]]
        dim_names <- names(res$dim_list)
        dimensions <- paste(dim_names, collapse = " x ")
        variables <- paste(names(res$variables), collapse = ", ")
        data.table::data.table(
          table_name = name,
          dimensions = dimensions,
          variables = variables
        )
      })

      return(data.table::rbindlist(table_info))
    }
  ),

  private = list(
    n_threads = 1,
    microdata = NULL, # Stores microdata with record_id, direction, direction_rebalanced, noise_multiplier
    pert_status = list(), # Cache for perturbation status (keyed by "target_var_name")

    # Track rebalancing status
    rebal_status = list(
      done = FALSE,
      dim_list = NULL,
      params = NULL
    ),

    # Store results from perturb() calls (hash-managed)
    result_tables = list(), # Named list: result_tables[[name]] = list with table, meta, variables

    log_info = function(msg) {
      if (Sys.getenv("SDC_LOG_LEVEL") != "OFF") {
        cli::cli_alert_info(msg, .envir = parent.frame())
      }
    },
    log_success = function(msg) {
      if (Sys.getenv("SDC_LOG_LEVEL") != "OFF") {
        cli::cli_alert_success(msg, .envir = parent.frame())
      }
    }
  )
)
