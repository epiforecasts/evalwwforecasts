#' Classify the epidemic phase over the forecast period
#'
#' Computes the percent change in the final (evaluation) hospital admissions
#' and wastewater concentrations between the forecast date and the end of
#' the forecast period, and classifies each as `"increasing"`,
#' `"decreasing"` or `"stable"`.
#'
#' - Hospital admissions: the 7-day rolling sum on the forecast date is
#'   compared with the 7-day rolling sum `horizon` days later.
#' - Wastewater: for each site and lab, the mean log concentration over the
#'   `window` days up to the forecast date is compared with the mean over the
#'   `window` days up to `horizon` days later. Sites and labs need at least one
#'   observation in both windows. The differences are averaged across sites,
#'   weighted by the population served, and converted to a percent change.
#'
#' A change greater than `threshold` percent is `"increasing"`, less than
#' `-threshold` percent is `"decreasing"`, and otherwise `"stable"`. The
#' percent changes are returned as well, so the phases can be reclassified
#' with a different threshold.
#'
#' @param hosp_data_eval Data.frame of the final hospital admissions data for
#'   the location, with `date` and `updated_hosp_7d_count`
#' @param ww_data_eval Data.frame of the final wastewater data for the
#'   location, as returned by `get_ww_for_eval()`, or `NULL`
#' @param forecast_date Forecast date
#' @param location_name Name of the location
#' @param horizon Integer, number of days after the forecast date to compare
#'   with. Default `28`.
#' @param threshold Numeric, percent change used to separate increasing and
#'   decreasing from stable. Default `5`.
#' @param window Integer, number of days averaged over for the wastewater
#'   data at each end. Default `7`, to match the 7-day hospital admissions.
#'
#' @returns One-row data.frame with `location`, `forecast_date`,
#'   `hosp_pct_change`, `hosp_phase`, `ww_pct_change`, `ww_n_sites` and
#'   `ww_phase`
#' @importFrom dplyr filter group_by summarise inner_join
#' @importFrom stats weighted.mean
#' @autoglobal
get_epidemic_phase <- function(hosp_data_eval,
                               ww_data_eval,
                               forecast_date,
                               location_name,
                               horizon = 28,
                               threshold = 5,
                               window = 7) {
  start_date <- as.Date(forecast_date)
  end_date <- start_date + horizon

  hosp_start <- hosp_data_eval$updated_hosp_7d_count[
    as.Date(hosp_data_eval$date) == start_date
  ]
  hosp_end <- hosp_data_eval$updated_hosp_7d_count[
    as.Date(hosp_data_eval$date) == end_date
  ]
  hosp_pct_change <- if (length(hosp_start) == 1 && length(hosp_end) == 1 &&
    isTRUE(hosp_start > 0)) {
    100 * (hosp_end / hosp_start - 1)
  } else {
    NA_real_
  }

  ww_pct_change <- NA_real_
  ww_n_sites <- 0L
  if (!is.null(ww_data_eval) && nrow(ww_data_eval) > 0) {
    ww_window_means <- function(window_end) {
      window_means <- ww_data_eval |>
        filter(
          as.Date(date) > window_end - window,
          as.Date(date) <= window_end
        ) |>
        group_by(site, lab) |>
        summarise(
          log_conc = mean(log_genome_copies_per_ml),
          site_pop = mean(site_pop),
          .groups = "drop"
        )
      return(window_means)
    }
    ww_change <- inner_join(
      ww_window_means(start_date),
      ww_window_means(end_date),
      by = c("site", "lab"),
      suffix = c("_start", "_end")
    )
    ww_n_sites <- nrow(ww_change)
    if (ww_n_sites > 0) {
      mean_log_change <- weighted.mean(
        ww_change$log_conc_end - ww_change$log_conc_start,
        w = ww_change$site_pop_start
      )
      ww_pct_change <- 100 * (exp(mean_log_change) - 1)
    }
  }

  epidemic_phase <- data.frame(
    location = location_name,
    forecast_date = start_date,
    hosp_pct_change = hosp_pct_change,
    hosp_phase = classify_phase(hosp_pct_change, threshold),
    ww_pct_change = ww_pct_change,
    ww_n_sites = ww_n_sites,
    ww_phase = classify_phase(ww_pct_change, threshold)
  )

  return(epidemic_phase)
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
classify_phase <- function(pct_change, threshold = 5) {
  phase <- case_when(
    is.na(pct_change) ~ NA_character_,
    pct_change > threshold ~ "increasing",
    pct_change < -threshold ~ "decreasing",
    TRUE ~ "stable"
  )

  return(phase)
}
