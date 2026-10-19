#' Add evaluation columns to the wastewater quantiles
#'
#' Adds the model identifiers, the horizon relative to the forecast date, a
#' `period` label and, if `ww_data_eval` is provided, the later observed
#' concentrations (`observed`, `observed_below_lod`). The `period` is:
#' - `"calibration"`: on or before the last observation for that site and lab
#'   available as of the forecast date (in-sample fit)
#' - `"nowcast"`: after that, up to the forecast date (not yet reported at the
#'   time of the forecast)
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
#'
#' @returns `ww_quantiles` with the additional columns
#' @importFrom dplyr mutate group_by ungroup case_when left_join select
#' @importFrom lubridate ymd
#' @autoglobal
add_ww_eval_cols <- function(ww_quantiles,
                             ww_data_eval,
                             forecast_date,
                             include_ww,
                             hosp_data_real_time) {
  ww_quantiles <- ww_quantiles |>
    mutate(
      model = "wwinference",
      include_ww = include_ww,
      hosp_data_real_time = hosp_data_real_time,
      site = as.character(site),
      lab = as.character(lab),
      horizon = as.numeric(ymd(date) - ymd(forecast_date))
    ) |>
    group_by(site, lab) |>
    mutate(
      last_obs_as_of = suppressWarnings(
        max(date[!is.na(log_genome_copies_per_ml)])
      )
    ) |>
    ungroup() |>
    mutate(
      period = case_when(
        horizon > 0 ~ "forecast",
        is.finite(last_obs_as_of) & date <= last_obs_as_of ~ "calibration",
        TRUE ~ "nowcast"
      )
    ) |>
    select(-last_obs_as_of)

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
#' only (the in-sample calibration period is not scored). Observations at or
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
#' @importFrom dplyr filter select
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
      model, include_ww, hosp_data_real_time,
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
#' @param hosp_phase Optional data.frame of hospital admissions phases by
#'   location and forecast date, from `get_hosp_phase()`, joined onto the
#'   scores
#' @param ww_phase Optional data.frame of wastewater phases by location,
#'   forecast date, site and lab, from `get_ww_phase()`, joined onto the
#'   scores
#'
#' @returns Data.frame of scores combined across runs
#' @importFrom dplyr bind_rows filter distinct left_join mutate
#' @importFrom readr read_csv
#' @autoglobal
score_ww_quantile_files <- function(scenarios,
                                    ind_filepath,
                                    metrics,
                                    hosp_phase = NULL,
                                    ww_phase = NULL) {
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

  if (nrow(scores_ww) > 0) {
    scores_ww <- mutate(
      scores_ww,
      forecast_date = as.Date(forecast_date),
      site = as.character(site),
      lab = as.character(lab)
    )
    if (!is.null(hosp_phase)) {
      scores_ww <- left_join(
        scores_ww, hosp_phase,
        by = c("location", "forecast_date")
      )
    }
    if (!is.null(ww_phase)) {
      scores_ww <- left_join(
        scores_ww, ww_phase,
        by = c("location", "forecast_date", "site", "lab")
      )
    }
  }

  return(scores_ww)
}
