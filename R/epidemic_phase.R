#' Get the hospital admissions epidemic phase over the forecast period
#'
#' Computes the percent and absolute change in the final (evaluation) 7-day
#' rolling sum of hospital admissions from the forecast date to `horizon` days
#' later, and classifies it with `classify_phase()`. A change counts as
#' increasing or decreasing only if it exceeds both `threshold` percent and
#' `abs_threshold` admissions, so that fluctuations at low counts are treated
#' as stable. When the count on the forecast date is 0 the percent change is
#' undefined (`NA`), and the phase is based on the absolute change alone.
#'
#' @param hosp_data_eval Data.frame of the final hospital admissions data for
#'   the location, with `date` and `updated_hosp_7d_count`
#' @param forecast_date Forecast date
#' @param location_name Name of the location
#' @param horizon Integer, number of days after the forecast date to compare
#'   with. Default `28`.
#' @param threshold Numeric, percent change separating increasing and
#'   decreasing from stable. Default `15`.
#' @param abs_threshold Numeric, change in the 7-day sum of admissions that an
#'   increase or decrease must also exceed. Default `5`.
#'
#' @returns One-row data.frame with `location`, `forecast_date`,
#'   `hosp_pct_change`, `hosp_abs_change` and `hosp_phase`
#' @autoglobal
get_hosp_phase <- function(hosp_data_eval,
                           forecast_date,
                           location_name,
                           horizon = 28,
                           threshold = 15,
                           abs_threshold = 5) {
  start_date <- as.Date(forecast_date)
  end_date <- start_date + horizon
  dates <- as.Date(hosp_data_eval$date)

  hosp_start <- hosp_data_eval$updated_hosp_7d_count[dates == start_date]
  hosp_end <- hosp_data_eval$updated_hosp_7d_count[dates == end_date]
  both_observed <- length(hosp_start) == 1 && length(hosp_end) == 1
  hosp_abs_change <- if (both_observed) hosp_end - hosp_start else NA_real_
  hosp_pct_change <- if (both_observed && isTRUE(hosp_start > 0)) {
    100 * (hosp_end / hosp_start - 1)
  } else {
    NA_real_
  }

  hosp_phase <- data.frame(
    location = location_name,
    forecast_date = start_date,
    hosp_pct_change = hosp_pct_change,
    hosp_abs_change = hosp_abs_change,
    hosp_phase = classify_phase(
      hosp_pct_change, threshold,
      abs_change = hosp_abs_change, abs_threshold = abs_threshold
    )
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
#' A change is increasing (decreasing) if the percent change is above
#' `threshold` (below `-threshold`) and, when `abs_change` is given, the
#' absolute change is also above `abs_threshold` (below `-abs_threshold`).
#' If the percent change is `NA` but the absolute change is known (e.g. a
#' starting count of 0), only the absolute change is used.
#'
#' @param pct_change Numeric vector of percent changes
#' @param threshold Numeric, percent change separating increasing and
#'   decreasing from stable
#' @param abs_change Optional numeric vector of absolute changes, the same
#'   length as `pct_change`. Default `NULL` (percent change only).
#' @param abs_threshold Numeric, absolute change that an increase or decrease
#'   must also exceed. Ignored if `abs_change` is `NULL`.
#'
#' @returns Character vector of `"increasing"`, `"decreasing"`, `"stable"` or
#'   `NA`
classify_phase <- function(pct_change,
                           threshold = 15,
                           abs_change = NULL,
                           abs_threshold = 5) {
  if (is.null(abs_change)) {
    abs_change <- rep(NA_real_, length(pct_change))
    abs_threshold <- -Inf
  }
  pct_up <- is.na(pct_change) | pct_change > threshold
  pct_down <- is.na(pct_change) | pct_change < -threshold
  abs_up <- is.na(abs_change) | abs_change > abs_threshold
  abs_down <- is.na(abs_change) | abs_change < -abs_threshold
  phase <- rep("stable", length(pct_change))
  phase[pct_up & abs_up] <- "increasing"
  phase[pct_down & abs_down] <- "decreasing"
  phase[is.na(pct_change) & is.na(abs_change)] <- NA_character_

  return(phase)
}
