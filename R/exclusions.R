#' Get the scores included in the analysis
#'
#' Excludes every location-forecast date where any model run was flagged for
#' MCMC convergence issues (`any_flag`) or where wastewater data were not
#' available (`flag_missing_ww`, in which case the wastewater model was fit
#' to hospital admissions only), for all models, so that every model is
#' evaluated on the same set of forecasts.
#'
#' @param scores Scores with one row per model, wastewater inclusion,
#'   location, forecast date and target date, including `any_flag`
#'
#' @returns Scores object (see `convert_to_su_object()`) of the included
#'   forecasts
#' @importFrom dplyr group_by filter ungroup
#' @autoglobal
get_scores_included <- function(scores) {
  scores_df <- as.data.frame(scores)
  if (anyNA(scores_df$any_flag) || anyNA(scores_df$flag_missing_ww)) {
    cli::cli_abort(
      "{.var any_flag} or {.var flag_missing_ww} is missing for some scores,
      so the exclusions cannot be applied."
    )
  }
  scores_included <- scores_df |>
    group_by(location, forecast_date) |>
    filter(!any(any_flag), !any(flag_missing_ww)) |>
    ungroup() |>
    convert_to_su_object()
  return(scores_included)
}

#' Get the location-forecast dates excluded from the analysis
#'
#' One row per location-forecast date excluded because any model run was
#' flagged for MCMC convergence issues or because wastewater data were not
#' available, with the flagged runs and which checks they failed.
#'
#' @param diagnostics Data.frame of MCMC diagnostics, one row per model run,
#'   as saved in `diagnostics.csv`
#'
#' @returns Data.frame with `location`, `forecast_date`, `flagged_runs`, a
#'   column for each convergence check that is `TRUE` if any run failed it,
#'   `flag_missing_ww` (`TRUE` if wastewater data were not available) and
#'   `reason`
#' @importFrom dplyr filter group_by summarise arrange mutate
#' @importFrom glue glue
#' @autoglobal
get_excluded_forecasts <- function(diagnostics) {
  excluded <- diagnostics |>
    filter(any_flag | flag_missing_ww) |>
    mutate(run = glue("{model}-{ifelse(include_ww, 'ww', 'hosp')}")) |>
    group_by(location, forecast_date) |>
    summarise(
      flagged_runs = paste(sort(unique(run[any_flag])), collapse = "; "),
      flag_high_max_treedepth = any(flag_high_max_treedepth),
      flag_too_many_divergences = any(flag_too_many_divergences),
      flag_high_rhat = any(flag_high_rhat),
      flag_low_ebfmi = any(flag_low_ebfmi),
      flag_missing_ww = any(flag_missing_ww),
      .groups = "drop"
    ) |>
    mutate(reason = dplyr::case_when(
      flag_missing_ww & flagged_runs != "" ~ "missing wastewater; convergence",
      flag_missing_ww ~ "missing wastewater",
      TRUE ~ "convergence"
    )) |>
    arrange(location, forecast_date)
  return(as.data.frame(excluded))
}

#' Check the excluded forecasts match the scores
#'
#' Warns if the location-forecast dates excluded based on the scores differ
#' from those excluded based on the diagnostics.
#'
#' @inheritParams get_scores_included
#' @param excluded_forecasts Data.frame from `get_excluded_forecasts()`
#'
#' @returns `excluded_forecasts`, unchanged
#' @importFrom dplyr filter distinct anti_join
#' @autoglobal
check_excluded_forecasts <- function(scores, excluded_forecasts) {
  flagged_in_scores <- as.data.frame(scores) |>
    filter(any_flag | flag_missing_ww) |>
    distinct(location, forecast_date)
  keys <- c("location", "forecast_date")
  only_scores <- anti_join(flagged_in_scores, excluded_forecasts, by = keys)
  only_diagnostics <- anti_join(
    excluded_forecasts, flagged_in_scores,
    by = keys
  )
  if (nrow(only_scores) > 0 || nrow(only_diagnostics) > 0) {
    cli::cli_warn(c(
      "The flagged location-forecast dates in the scores and diagnostics
      differ.",
      "i" = "{nrow(only_scores)} flagged only in the scores and
      {nrow(only_diagnostics)} only in the diagnostics."
    ))
  }
  return(excluded_forecasts)
}
