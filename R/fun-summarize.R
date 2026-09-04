# Perturbation impact summary, shared between the functional API
# (rn_summarize) and the R6 engine ($summarize).

# Internal helper: compute and display the summary for one result entry.
.summarize_entry <- function(entry, table_label, target_vars = NULL) {
  res <- entry$table

  # Determine which variables to summarize
  if (is.null(target_vars)) {
    target_vars_all <- names(entry$variables)
  } else {
    target_vars_all <- target_vars
  }

  # Validate target variables exist
  missing <- setdiff(target_vars_all, names(entry$variables))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "x" = "Variable(s) {.val {missing}} not found in table {.val {table_label}}.",
      "i" = "Available variables: {.val {names(entry$variables)}}"
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
    cli::cli_h1("EZS Perturbation Summary: {.val {table_label}}")
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

  invisible(result_list)
}

#' Summarize Perturbation Impact (Functional API)
#'
#' @description
#' Summarize perturbation impact with a 3-way comparison (original, initial,
#' final). This is the functional counterpart of the R6 `$summarize()` method.
#'
#' @param x An `rn_perturbed` object (from [rn_perturb()] or
#'   `sdc$get_table()`).
#' @param target_vars Optional character vector of variable names to summarize.
#'   If NULL, summarizes all variables in the table.
#'
#' @return Invisibly returns a named list of summary statistics (one per
#'   variable).
#'
#' @export
#' @examples
#' old_log <- Sys.getenv("SDC_LOG_LEVEL")
#' Sys.setenv(SDC_LOG_LEVEL = "OFF")
#' dt <- data.table::data.table(
#'   country = sample(c("AT", "DE", "NL"), 100, replace = TRUE),
#'   turnover = runif(100, 10, 1000),
#'   direction = sample(c(1, -1), 100, replace = TRUE),
#'   noise_multiplier = 0.05
#' )
#' dims <- list(country = sdcHierarchies::hier_create("Total",
#'   nodes = c("AT", "DE", "NL")
#' ))
#' res <- dt |>
#'   rn_init(sensitive_params = list(n_threshold = 3)) |>
#'   rn_rebalance(dim_list = dims, num_var = "turnover") |>
#'   rn_perturb(dim_list = dims, variables = "turnover")
#' summary_stats <- rn_summarize(res)
#' Sys.setenv(SDC_LOG_LEVEL = old_log)
rn_summarize <- function(x, target_vars = NULL) {
  .assert_rn_class(
    x,
    "rn_perturbed",
    hint = "Call {.fn rn_perturb} first to create a result table."
  )

  .summarize_entry(x, table_label = "rn_perturbed", target_vars = target_vars)
}
