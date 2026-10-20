#' Get MCMC convergence diagnostics for a wwinference fit
#'
#' Summarises the sampler diagnostics of a single model run into one row:
#' divergences, max tree depth hits, E-BFMI, R-hat, effective sample sizes and
#' run time. Flags use the same default thresholds as
#' `wwinference::get_model_diagnostic_flags()`, so `any_flag` matches what
#' that function would report.
#'
#' @param ww_fit_obj Fitted object returned by `wwinference::wwinference()`
#' @param ebmfi_tolerance Mean E-BFMI at or below which the run is flagged
#' @param divergences_tolerance Proportion of divergent transitions (in any
#'   chain) at or above which the run is flagged
#' @param frac_high_rhat_tolerance Proportion of quantities with R-hat above
#'   `rhat_tolerance` at or above which the run is flagged
#' @param rhat_tolerance R-hat threshold
#' @param max_tree_depth_tol Proportion of transitions hitting max tree depth
#'   (in any chain) at or above which the run is flagged
#'
#' @returns One-row data.frame of diagnostics
#' @importFrom dplyr mutate
#' @autoglobal
get_fit_diagnostics <- function(ww_fit_obj,
                                ebmfi_tolerance = 0.2,
                                divergences_tolerance = 0.01,
                                frac_high_rhat_tolerance = 0.05,
                                rhat_tolerance = 1.05,
                                max_tree_depth_tol = 0.01) {
  fit <- ww_fit_obj$fit$result
  n_chains <- fit$num_chains()
  iter_sampling <- fit$metadata()$iter_sampling
  iter_warmup <- fit$metadata()$iter_warmup

  sampler_diag <- fit$diagnostic_summary(quiet = TRUE)
  param_summary <- fit$summary(
    variables = NULL,
    "rhat", "ess_bulk", "ess_tail"
  )

  rhat <- param_summary$rhat
  diagnostics <- data.frame(
    n_chains = n_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    n_divergent = sum(sampler_diag$num_divergent),
    max_n_divergent_chain = max(sampler_diag$num_divergent),
    n_max_treedepth = sum(sampler_diag$num_max_treedepth),
    min_ebfmi = min(sampler_diag$ebfmi),
    mean_ebfmi = mean(sampler_diag$ebfmi),
    max_rhat = max(rhat, na.rm = TRUE),
    frac_high_rhat = mean(rhat > rhat_tolerance, na.rm = TRUE),
    min_ess_bulk = min(param_summary$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(param_summary$ess_tail, na.rm = TRUE),
    run_time_secs = fit$time()$total
  )

  diagnostics <- diagnostics |>
    mutate(
      flag_high_max_treedepth = max(sampler_diag$num_max_treedepth) >=
        n_chains * iter_sampling * max_tree_depth_tol,
      flag_too_many_divergences = max_n_divergent_chain >=
        n_chains * iter_sampling * divergences_tolerance,
      flag_high_rhat = frac_high_rhat >= frac_high_rhat_tolerance,
      flag_low_ebfmi = mean_ebfmi <= ebmfi_tolerance,
      any_flag = flag_high_max_treedepth | flag_too_many_divergences |
        flag_high_rhat | flag_low_ebfmi
    )

  return(diagnostics)
}

#' Extract one row of MCMC diagnostics per model run
#'
#' `fit_wwinference_wrapper()` attaches each run's diagnostics to its
#' quantiles as columns prefixed with `diag_`. This collapses them back to one
#' row per forecast date, location and model and drops the prefix.
#'
#' @param hosp_quantiles Quantiles returned by `fit_wwinference_wrapper()`,
#'   combined across runs
#'
#' @returns data.frame with one row per model run
#' @importFrom dplyr distinct across starts_with rename_with as_tibble
#' @autoglobal
extract_fit_diagnostics <- function(hosp_quantiles) {
  # Drop the scoringutils `forecast` class first (unclass, so the `forecast`
  # package's as.data.frame() method is never called); scoringutils otherwise
  # warns when a subset is no longer a valid forecast
  diagnostics <- as_tibble(unclass(hosp_quantiles)) |>
    distinct(
      model, include_ww, hosp_data_real_time, location, forecast_date,
      flag_missing_ww, across(starts_with("diag_"))
    ) |>
    rename_with(function(x) sub("^diag_", "", x), starts_with("diag_"))

  return(diagnostics)
}
