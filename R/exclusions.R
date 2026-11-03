#' Get the scores included in the analysis
#'
#' Excludes every location-forecast date where any model run was flagged for
#' MCMC convergence issues (`any_flag`), for all models, so that every model
#' is evaluated on the same set of forecasts.
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
  if (anyNA(scores_df$any_flag)) {
    cli::cli_abort(
      "{.var any_flag} is missing for some scores, so the convergence
      exclusions cannot be applied."
    )
  }
  scores_included <- scores_df |>
    group_by(location, forecast_date) |>
    filter(!any(any_flag)) |>
    ungroup() |>
    convert_to_su_object()
  return(scores_included)
}

#' Get the location-forecast dates excluded for failing convergence checks
#'
#' One row per location-forecast date where any model run was flagged for
#' MCMC convergence issues, with the flagged runs and which checks they
#' failed.
#'
#' @param diagnostics Data.frame of MCMC diagnostics, one row per model run,
#'   as saved in `diagnostics.csv`
#'
#' @returns Data.frame with `location`, `forecast_date`, `flagged_runs`, a
#'   column for each convergence check that is `TRUE` if any run failed it,
#'   and `flag_missing_ww` (`TRUE` if the location-forecast date was also
#'   missing wastewater data)
#' @importFrom dplyr filter group_by summarise arrange mutate
#' @importFrom glue glue
#' @autoglobal
get_excluded_forecasts <- function(diagnostics) {
  excluded <- diagnostics |>
    filter(any_flag) |>
    mutate(run = glue("{model}-{ifelse(include_ww, 'ww', 'hosp')}")) |>
    group_by(location, forecast_date) |>
    summarise(
      flagged_runs = paste(sort(unique(run)), collapse = "; "),
      flag_high_max_treedepth = any(flag_high_max_treedepth),
      flag_too_many_divergences = any(flag_too_many_divergences),
      flag_high_rhat = any(flag_high_rhat),
      flag_low_ebfmi = any(flag_low_ebfmi),
      flag_missing_ww = any(flag_missing_ww),
      .groups = "drop"
    ) |>
    arrange(location, forecast_date)
  return(as.data.frame(excluded))
}

#' Check the excluded forecasts match the scores
#'
#' Warns if the location-forecast dates flagged in the scores differ from
#' those flagged in the diagnostics.
#'
#' @inheritParams get_scores_included
#' @param excluded_forecasts Data.frame from `get_excluded_forecasts()`
#'
#' @returns `excluded_forecasts`, unchanged
#' @importFrom dplyr filter distinct anti_join
#' @autoglobal
check_excluded_forecasts <- function(scores, excluded_forecasts) {
  flagged_in_scores <- as.data.frame(scores) |>
    filter(any_flag) |>
    distinct(location, forecast_date)
  keys <- c("location", "forecast_date")
  only_scores <- anti_join(flagged_in_scores, excluded_forecasts, by = keys)
  # Location-forecast dates missing wastewater are removed from the scores
  # before this, so are not expected in them
  only_diagnostics <- anti_join(
    filter(excluded_forecasts, !flag_missing_ww), flagged_in_scores,
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
