#' @rdname rn_setup
rebalancedNoise <- R6Class(
  "rebalancedNoise_Class",
  public = list(
    #' @field sensitive_params Parameters for SDC rules (e.g., n_threshold, p_rule)
    sensitive_params = NULL,

    #' @description
    #' Initialize the SDC engine from an `rn_initialized` state object
    #' (created by [rn_init()]).
    #' @param state An `rn_initialized` object from [rn_init()].
    initialize = function(state) {
      private$.state <- state
      self$sensitive_params <- state$sensitive_params
      private$n_threads <- state$n_threads
      private$microdata <- state$microdata

      # Initialize status tracking
      private$rebal_status <- state$rebal_status

      # Initialize result tables (tag entries for the functional API)
      if (length(state$result_tables) > 0) {
        private$result_tables <- lapply(state$result_tables, .as_rn_perturbed)
      }
    },

    #' @description
    #' Perform rebalancing ONCE and store updated direction values in microdata.
    #' Delegates to [rn_rebalance()]. This can only be performed once per
    #' object; call `$reset()` first to start over with a different
    #' structure or variable.
    #' @param dim_list Named list of hierarchies defining the detailed table structure
    #'   for rebalancing (e.g., `list(nace = hier_detailed, bgkl = hier_detailed)`).
    #' @param num_var Name of the numerical variable to rebalance.
    #'   Currently only a single variable is supported.
    #' @return Invisibly returns the object (for chaining).
    rebalance = function(dim_list, num_var) {
      if (isTRUE(private$rebal_status$done)) {
        cli::cli_abort(c(
          "x" = "Rebalancing has already been performed on this object.",
          "i" = "Call {.fn reset} first to start over with a different structure or variable."
        ))
      }

      private$.state <- rn_rebalance(
        private$.state,
        dim_list = dim_list,
        num_var = num_var
      )

      # Update microdata with rebalanced directions
      private$microdata <- private$.state$microdata

      # Update rebalancing status
      private$rebal_status <- list(
        done = TRUE,
        dim_list = dim_list,
        params = self$sensitive_params,
        num_var = num_var
      )

      return(invisible(self))
    },

    #' @description
    #' Reset the object to its initialized state, allowing a fresh
    #' `$rebalance()` with a different structure or variable. Removes all rebalancing artifacts (`direction_rebalanced`,
    #' `strID`, `is_sens_*` columns) from the microdata, restores the original
    #' row order and clears all cached perturbation results.
    #' @param sensitive_params Optional list of SDC rules replacing
    #'   `self$sensitive_params`. If `NULL` (default), the current parameters
    #'   are kept.
    #' @return Invisibly returns the object (for chaining).
    reset = function(sensitive_params = NULL) {
      if (!is.null(sensitive_params)) {
        if (!is.list(sensitive_params)) {
          cli::cli_abort("{.arg sensitive_params} must be a list.")
        }
        self$sensitive_params <- sensitive_params
      }

      # Strip rebalancing artifacts and restore original row order
      dt <- private$microdata
      if (!is.null(dt)) {
        drop_cols <- intersect(
          c(
            "direction_rebalanced",
            "strID",
            grep("^is_sens_", names(dt), value = TRUE)
          ),
          names(dt)
        )
        if (length(drop_cols) > 0) {
          dt[, (drop_cols) := NULL]
        }
        if ("record_id" %in% names(dt)) {
          setorder(dt, record_id)
        }
      }
      private$microdata <- dt

      # Reset status tracking and caches
      private$rebal_status <- list(
        done = FALSE,
        dim_list = NULL,
        params = NULL,
        num_var = NULL
      )
      private$result_tables <- list()
      private$pert_status <- list()

      # Rebuild the functional-API state as freshly initialized
      private$.state <- .new_rn_initialized(
        microdata = dt,
        sensitive_params = self$sensitive_params,
        n_threads = private$n_threads,
        rebal_status = private$rebal_status,
        result_tables = list()
      )

      private$log_success(
        "Reset complete. Call {.fn rebalance} to start over."
      )
      return(invisible(self))
    },

    #' @description
    #' Execute the EZS perturbation for specific variables using stored directions.
    #' Uses the same core as [rn_perturb()] plus caching and table management.
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

      .validate_perturb_args(private$microdata, dim_list, variables, round)

      # Validate name parameter
      if (is.null(name) || !is.character(name) || length(name) != 1) {
        cli::cli_abort("{.arg name} must be a single character string.")
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

      dim_names <- names(dim_list)

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

        # Perturb and tabulate this variable (shared core with rn_perturb)
        tab_res <- .perturb_tabulate(
          microdata = private$microdata,
          dim_list = dim_list,
          target_var = tv,
          sensitive_params = curr_params,
          round = round,
          n_threads = private$n_threads
        )
        full_res <- tab_res$table
        base_cells <- tab_res$base_cells
        init_name <- paste0(tv, "_init")
        p_name <- paste0(tv, "_pert")
        sens_col_name <- paste0("is_sens_", tv)

        # Create result entry structure (only for first variable)
        if (tv == variables[1]) {
          result_entry <- .new_rn_perturbed(
            table = full_res,
            hash = table_hash,
            dim_list = dim_list,
            sensitive_params = curr_params,
            round = round,
            base_cells = base_cells[, .(strID, is_base_cell)],
            variables = list()
          )
        }

        if (tv == variables[1] && !(name %in% names(private$result_tables))) {
          # New table - store entry
          private$result_tables[[name]] <- result_entry

          private$log_success(
            "Created new table {.val {name}} with variable {.var {tv}}."
          )
        } else {
          # Merge new columns into the existing table
          private$result_tables[[name]]$table <- .merge_var_into_table(
            existing_table = private$result_tables[[name]]$table,
            full_res = full_res,
            dim_names = dim_names,
            cols_to_add = c(tv, init_name, p_name, sens_col_name),
            reorder = tv == variables[1]
          )

          private$log_success(
            "Added {.var {tv}} to existing table {.val {name}}."
          )
        }

        # Add variable to metadata
        private$result_tables[[name]]$variables[[tv]] <- list(
          sensitive_params = curr_params,
          is_sens_col = sens_col_name
        )

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

      # If name is NULL, return all results as named list
      if (is.null(name)) {
        result_list <- lapply(private$result_tables, function(res) {
          .process_result_table(
            copy(res$table),
            res$variables,
            format = format,
            variables = variables
          )
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
      result <- .process_result_table(
        copy(stored_result$table),
        stored_result$variables,
        format = format,
        variables = variables
      )
      return(result)
    },

    #' @description
    #' Summarize perturbation impact with 3-way comparison. Delegates to
    #' the shared core of [rn_summarize()].
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

      .summarize_entry(
        private$result_tables[[table]],
        table_label = table,
        target_vars = target_vars
      )
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
        private$log_success("Exported to {.path {file}}")
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
    #' Get the current pipeline state as an S3 object for use with the
    #' functional API ([rn_rebalance()], [rn_perturb()]).
    #' @return An `rn_rebalanced` object if rebalancing has been performed
    #'   (or imported as completed), otherwise an `rn_initialized` object.
    get_state = function() {
      st <- private$.state
      if (
        inherits(st, "rn_initialized") &&
          isTRUE(st$rebal_status$done)
      ) {
        return(.promote_rn_rebalanced(st))
      }
      st
    },

    #' @description
    #' Get a stored result table as an `rn_perturbed` S3 object for use with
    #' the functional API ([rn_format()], [rn_summarize()]).
    #' @param name Character name of the result table.
    #' @return An `rn_perturbed` object.
    get_table = function(name) {
      if (!(name %in% names(private$result_tables))) {
        available_names <- paste(names(private$result_tables), collapse = ", ")
        cli::cli_abort(c(
          "x" = "Result table {.val {name}} not found.",
          "i" = "Available tables: {.val {available_names}}"
        ))
      }
      res <- private$result_tables[[name]]
      res$table <- copy(res$table)
      res$base_cells <- copy(res$base_cells)
      res
    },

    #' @description
    #' List available perturbed tables with their dimension labels and variables.
    #' @return A `data.table` with columns:
    #'   - `table_name`: Character name of the perturbed table
    #'   - `dimensions`: Dimension hierarchy labels joined with " x "
    #'   - `variables`: Comma-separated list of perturbed variables
    list_tables = function() {
      if (length(private$result_tables) == 0) {
        private$log_warning(
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
    n_threads = 1L,
    .state = NULL, # rn_initialized / rn_rebalanced S3 state object
    microdata = NULL, # Stores microdata with record_id, direction, direction_rebalanced, noise_multiplier
    pert_status = list(), # Cache for perturbation status (keyed by "target_var_name")

    # Track rebalancing status
    rebal_status = list(
      done = FALSE,
      dim_list = NULL,
      params = NULL,
      num_var = NULL
    ),

    # Store results from perturb() calls (hash-managed, as rn_perturbed objects)
    result_tables = list(), # Named list: result_tables[[name]] = rn_perturbed object

    log_info = function(msg) {
      .rn_log_info(msg, envir = parent.frame())
    },
    log_success = function(msg) {
      .rn_log_success(msg, envir = parent.frame())
    },
    log_warning = function(msg) {
      .rn_log_warning(msg, envir = parent.frame())
    }
  )
)
