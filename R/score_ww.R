#' Add evaluation columns to the wastewater quantiles
#'
#' Adds the model identifiers, the run's MCMC convergence flag (`any_flag`),
#' the horizon relative to the forecast date, a `period` label and, if
#' `ww_data_eval` is provided, the later observed concentrations (`observed`,
#' `observed_below_lod`). The `period` of each site, lab and date is:
#' - `"calibration"`: on or before the forecast date, with an observation in
#'   the data available as of the forecast date (used to fit the model)
#' - `"nowcast"`: on or before the forecast date, without an observation in
#'   the data available as of the forecast date. This includes dates after the
#'   last fit-time observation and dates that were backfilled later.
#' - `"forecast"`: after the forecast date
#'
#' @param ww_quantiles Data.frame of wastewater quantiles joined to the
#'   wastewater data available as of the forecast date
#' @param ww_data_eval Data.frame of the final wastewater data for the
#'   location, as returned by `get_ww_for_eval()`, or `NULL`
#' @param forecast_date Forecast date
#' @param include_ww Logical, whether the model included wastewater
#' @param hosp_data_real_time Logical, whether real-time hospital data were
#'   used
#' @param any_flag Logical, whether the model run was flagged for convergence
#'   issues by `get_fit_diagnostics()`
#'
#' @returns `ww_quantiles` with the additional columns
#' @importFrom dplyr mutate case_when left_join
#' @importFrom lubridate ymd
#' @autoglobal
add_ww_eval_cols <- function(ww_quantiles,
                             ww_data_eval,
                             forecast_date,
                             include_ww,
                             hosp_data_real_time,
                             any_flag) {
  ww_quantiles <- ww_quantiles |>
    mutate(
      model = "wwinference",
      include_ww = include_ww,
      hosp_data_real_time = hosp_data_real_time,
      any_flag = any_flag,
      site = as.character(site),
      lab = as.character(lab),
      horizon = as.numeric(ymd(date) - ymd(forecast_date)),
      # Calibration only where the date had an observation at fit time, so
      # observations that were not available then are always scored
      period = case_when(
        horizon > 0 ~ "forecast",
        !is.na(log_genome_copies_per_ml) ~ "calibration",
        TRUE ~ "nowcast"
      )
    )

  if (!is.null(ww_data_eval) && nrow(ww_data_eval) > 0) {
    ww_quantiles <- left_join(
      ww_quantiles,
      get_ww_eval_obs(ww_data_eval),
      by = c("date", "site", "lab")
    )
  }

  return(ww_quantiles)
}

#' Get the later observed wastewater concentrations for evaluation
#'
#' Formats the final wastewater dataset so it can be joined onto the
#' wastewater predictions by site, lab and date. Observations at or below the
#' limit of detection are flagged, using the same definition as
#' `wwinference::preprocess_ww_data()`.
#'
#' @param ww_data_eval Data.frame of the final wastewater data for a location,
#'   as returned by `get_ww_for_eval()`
#'
#' @returns Data.frame with one row per date, site and lab, with columns
#'   `observed` (log genome copies per mL) and `observed_below_lod`
#' @importFrom dplyr mutate group_by summarise
#' @autoglobal
get_ww_eval_obs <- function(ww_data_eval) {
  ww_eval_obs <- ww_data_eval |>
    mutate(
      date = as.Date(date),
      site = as.character(site),
      lab = as.character(lab)
    ) |>
    group_by(date, site, lab) |>
    summarise(
      observed_below_lod = any(log_genome_copies_per_ml <= log_lod),
      observed = mean(log_genome_copies_per_ml),
      .groups = "drop"
    )

  return(ww_eval_obs)
}

#' Score wastewater predictions against the later observed data
#'
#' Scores the quantiles of the predicted wastewater concentrations (log genome
#' copies per mL) against the later observed concentrations, for every site,
#' lab and date with a later observation, for the nowcast and forecast periods
#' only, i.e. observations that were not available when the model was fitted
#' (see `add_ww_eval_cols()`). Observations at or
#' below the limit of detection are excluded. Predictions are already on the
#' log scale, so no transformation is applied. Use `period` to separate the
#' nowcast and forecast.
#'
#' @param ww_quantiles Data.frame of wastewater quantiles with evaluation
#'   columns, as saved in `ww_quantiles.csv`
#' @param metrics Scoring metrics passed to `scoringutils::score()`
#'
#' @returns Data.frame of scores with one row per site, lab, date and
#'   forecast, or `NULL` if there are no observations to score against
#' @importFrom dplyr filter select any_of
#' @importFrom scoringutils as_forecast_quantile score
#' @autoglobal
score_ww_quantiles <- function(ww_quantiles, metrics) {
  if (!"observed" %in% names(ww_quantiles)) {
    return(NULL)
  }
  to_score <- ww_quantiles |>
    filter(
      period != "calibration",
      !is.na(observed),
      !observed_below_lod
    ) |>
    select(
      model, include_ww, hosp_data_real_time, any_of("any_flag"),
      location = location_name,
      forecast_date, site, lab, date, horizon, period,
      quantile_level, predicted, observed
    )

  if (nrow(to_score) == 0) {
    return(NULL)
  }

  ww_scores <- to_score |>
    as_forecast_quantile() |>
    score(metrics = metrics) |>
    as.data.frame()

  return(ww_scores)
}

#' Score the saved wastewater quantiles from every model run
#'
#' Reads `ww_quantiles.csv` for each wastewater scenario (forecast date and
#' location) and scores it with `score_ww_quantiles()`. Files are scored one
#' at a time so only the scores are held in memory. Scenarios without a file
#' (e.g. no wastewater data available) are skipped.
#'
#' @param scenarios Data.frame of model run scenarios, with columns
#'   `forecast_date`, `location_name` and `include_ww`
#' @param ind_filepath Character string of the directory the model run
#'   outputs were saved to
#' @param metrics Scoring metrics passed to `scoringutils::score()`
#'
#' @returns Data.frame of scores combined across runs
#' @importFrom dplyr bind_rows filter distinct
#' @importFrom readr read_csv
#' @autoglobal
score_ww_quantile_files <- function(scenarios, ind_filepath, metrics) {
  ww_runs <- scenarios |>
    filter(include_ww) |>
    distinct(forecast_date, location_name)
  files <- file.path(
    ind_filepath, ww_runs$forecast_date, ww_runs$location_name,
    "data", "ww_quantiles.csv"
  )
  files <- files[file.exists(files)]

  scores_ww <- bind_rows(lapply(files, function(f) {
    ww_quantiles <- read_csv(f, show_col_types = FALSE)
    return(score_ww_quantiles(ww_quantiles, metrics = metrics))
  }))

  return(scores_ww)
}
