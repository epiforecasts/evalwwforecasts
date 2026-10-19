#' Get the hospital admissions epidemic phase over the forecast period
#'
#' Computes the percent change in the final (evaluation) 7-day rolling sum of
#' hospital admissions from the forecast date to `horizon` days later, and
#' classifies it with `classify_phase()`.
#'
#' @param hosp_data_eval Data.frame of the final hospital admissions data for
#'   the location, with `date` and `updated_hosp_7d_count`
#' @param forecast_date Forecast date
#' @param location_name Name of the location
#' @param horizon Integer, number of days after the forecast date to compare
#'   with. Default `28`.
#' @param threshold Numeric, percent change separating increasing and
#'   decreasing from stable. Default `15`.
#'
#' @returns One-row data.frame with `location`, `forecast_date`,
#'   `hosp_pct_change` and `hosp_phase`
#' @autoglobal
get_hosp_phase <- function(hosp_data_eval,
                           forecast_date,
                           location_name,
                           horizon = 28,
                           threshold = 15) {
  start_date <- as.Date(forecast_date)
  end_date <- start_date + horizon
  dates <- as.Date(hosp_data_eval$date)

  hosp_start <- hosp_data_eval$updated_hosp_7d_count[dates == start_date]
  hosp_end <- hosp_data_eval$updated_hosp_7d_count[dates == end_date]
  hosp_pct_change <- if (length(hosp_start) == 1 && length(hosp_end) == 1 &&
    isTRUE(hosp_start > 0)) {
    100 * (hosp_end / hosp_start - 1)
  } else {
    NA_real_
  }

  hosp_phase <- data.frame(
    location = location_name,
    forecast_date = start_date,
    hosp_pct_change = hosp_pct_change,
    hosp_phase = classify_phase(hosp_pct_change, threshold)
  )

  return(hosp_phase)
}

#' Get the wastewater epidemic phase at each site over the forecast period
#'
#' For each site and lab, computes the percent change in the final
#' (evaluation) wastewater concentration between the `window` days up to the
#' forecast date and the `window` days up to `horizon` days later, using the
#' mean log concentration in each window, and classifies it with
#' `classify_phase()`. Only sites and labs with at least one observation in
#' both windows are returned.
#'
#' @param ww_data_eval Data.frame of the final wastewater data for the
#'   location, as returned by `get_ww_for_eval()`, or `NULL`
#' @inheritParams get_hosp_phase
#' @param window Integer, number of days averaged over at each end. Default
#'   `7`, to match the 7-day hospital admissions.
#'
#' @returns Data.frame with one row per site and lab, with `location`,
#'   `forecast_date`, `site`, `lab`, `ww_pct_change` and `ww_phase` (no rows
#'   if there are no wastewater data)
#' @importFrom dplyr filter group_by summarise inner_join mutate select
#' @autoglobal
get_ww_phase <- function(ww_data_eval,
                         forecast_date,
                         location_name,
                         horizon = 28,
                         threshold = 15,
                         window = 7) {
  if (is.null(ww_data_eval) || nrow(ww_data_eval) == 0) {
    empty <- data.frame(
      location = character(), forecast_date = as.Date(character()),
      site = character(), lab = character(),
      ww_pct_change = numeric(), ww_phase = character(),
      stringsAsFactors = FALSE
    )
    return(empty)
  }
  start_date <- as.Date(forecast_date)
  end_date <- start_date + horizon

  window_means <- function(window_end) {
    means <- ww_data_eval |>
      mutate(
        date = as.Date(date),
        site = as.character(site),
        lab = as.character(lab)
      ) |>
      filter(date > window_end - window, date <= window_end) |>
      group_by(site, lab) |>
      summarise(
        log_conc = mean(log_genome_copies_per_ml),
        .groups = "drop"
      )
    return(means)
  }

  ww_phase <- inner_join(
    window_means(start_date),
    window_means(end_date),
    by = c("site", "lab"),
    suffix = c("_start", "_end")
  ) |>
    mutate(
      location = location_name,
      forecast_date = start_date,
      ww_pct_change = 100 * (exp(log_conc_end - log_conc_start) - 1),
      ww_phase = classify_phase(ww_pct_change, threshold)
    ) |>
    select(location, forecast_date, site, lab, ww_pct_change, ww_phase)

  return(ww_phase)
}

#' Classify a percent change as increasing, decreasing or stable
#'
#' @param pct_change Numeric vector of percent changes
#' @param threshold Numeric, percent change separating increasing and
#'   decreasing from stable
#'
#' @returns Character vector of `"increasing"`, `"decreasing"`, `"stable"` or
#'   `NA`
#' @importFrom dplyr case_when
classify_phase <- function(pct_change, threshold = 15) {
  phase <- case_when(
    is.na(pct_change) ~ NA_character_,
    pct_change > threshold ~ "increasing",
    pct_change < -threshold ~ "decreasing",
    TRUE ~ "stable"
  )

  return(phase)
}
