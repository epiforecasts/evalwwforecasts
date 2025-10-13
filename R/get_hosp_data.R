#' Get the hospital admissions data
#'
#' @param location_name Character string indicating long form name of state.
#' @param location_abbr Character string indicating abbreviation of state
#' @param forecast_date Character string or date indicating the date of
#'    forecast in YYYY-MM-DD
#' @param forecast_horizon Forecast horizon. Default is `28`.
#' @param filepath_name Name of directory to save the raw input wastewater data.
#' @autoglobal
#' @importFrom dplyr rename mutate filter arrange desc pull
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom lubridate ymd days
get_hosp_for_eval <- function(location_name,
                              location_abbr,
                              forecast_date,
                              forecast_horizon = 28,
                              filepath_name =
                                file.path("input", "data", "hosp")) {
  if (file.exists(file.path(filepath_name, "RKI_hosp_adj.csv"))) {
    RKI_hosp_adj <- read_csv(file.path(filepath_name, "RKI_hosp_adj.csv"))
  } else {
    RKI_hosp_adj <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv") # nolint
    dir_create(filepath_name)
    write_csv(RKI_hosp_adj, file.path(filepath_name, "RKI_hosp_adj.csv"))
    # load in initial values
  }
  # Add a month to get out of the holiday period.
  date_to_get_init_vals <- min(RKI_hosp_adj$Datum) + days(30)
  init_vals <- get_initial_values(
    start_date_RKI_data = date_to_get_init_vals
  ) |>
    filter(age_group == "00+") |>
    mutate(location = stringr::str_sub(location, -2)) |>
    filter(location == location_abbr) |>
    arrange(-desc(date)) |>
    pull(value)

  hosp_clean <- RKI_hosp_adj |>
    rename(
      date = Datum,
      state = Bundesland,
      age_group = Altersgruppe,
      # This is the initial reported hospital admissions (right-truncated)
      init_hosp_7d_count = `fixierte_7T_Hospitalisierung_Faelle`,
      # This is the updated, eventual reported admissions for that date
      updated_hosp_7d_count = `aktualisierte_7T_Hospitalisierung_Faelle`,
      state_pop = `Bevoelkerung`
    ) |>
    arrange(-desc(date)) |>
    filter(
      state == location_name,
      date >= date_to_get_init_vals
    ) |>
    # Replace with once we have initial values
    mutate(
      daily_hosp_admits = convert_rolling_sum_to_inc(
        rolling_sums = updated_hosp_7d_count,
        k = 7,
        initial_values = init_vals
      ),
      forecast_date = forecast_date
    ) |>
    filter(
      date <= ymd(forecast_date),
      state == location_name
    ) |>
    select(
      date, state, forecast_date, daily_hosp_admits,
      state_pop, init_hosp_7d_count,
      updated_hosp_7d_count
    )
  return(hosp_clean)
}

#' Filter hospital admissions data for fitting
#'
#' @param hosp_data_eval hospital admissions data for evaluation step
#' @param forecast_date Character string or date indicating the date of
#'    forecast in YYYY-MM-DD
#' @param right_trunc Boolean indicating whether to use the real-time, right
#'    truncated data or the final corrected data. Default is `FALSE` indicating
#'    we will just use the corrected data.
#' @param calibration_period Integer indicating the number of days of
#'    hospital admissions calibration data to extract. Default is `100`.
#' @param lag Integer indicating the number of days from the forecast date of
#'    the latest hospital admission. Default is `3`.
#' @autoglobal
#' @importFrom dplyr filter
#' @importFrom lubridate ymd days

get_hosp_for_fit <- function(hosp_data_eval,
                             forecast_date,
                             right_trunc = FALSE,
                             calibration_period = 100,
                             lag = 3) {
  if (isFALSE(right_trunc)) {
    hosp_for_fit <- hosp_data_eval |>
      filter(
        date >= ymd(forecast_date) - days(calibration_period),
        date <= ymd(forecast_date) - days(lag)
      )
  } else {
    # Insert function to use git history to get the data as of the forecast date
    hosp_for_fit <- NULL
  }
  return(hosp_for_fit)
}

#' Get initial values of daily admissions
#'
#' @description This function uses the reporting triangle from the German
#'   Nowcast Hub to get the daily counts by reference date from the RKI data
#'   which is provided as 7-day sums. It does so my summing over all of the
#'   delays at each reference date.
#'
#'
#' @param start_date_RKI_data Character string indicating the date that the
#'    RKI hospitalization data begins, in YYYY-MM-DD format.
#' @param deconvolved_data_url Character string of the url for the reporting
#'    triangle from the German Nowcast Hub. Default is from the data at
#'    https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv #nolint
#' @autoglobal
#' @inheritParams get_hosp_for_eval
#' @importFrom glue glue
#' @importFrom dplyr select filter mutate arrange desc
#' @importFrom fs dir_create
#' @importFrom readr read_csv write_csv
get_initial_values <- function(
    start_date_RKI_data = "2023-01-02",
    deconvolved_data_url = "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv", # nolint
    filepath_name = file.path("input", "data", "hosp")) { # nolint
  if (file.exists(file.path(
    filepath_name,
    file.path(
      filepath_name,
      glue::glue("initial_values_{start_date_RKI_data}.csv")
    )
  ))) {
    init_vals <- read_csv(file.path(
      filepath_name,
      glue::glue("initial_values_{start_date_RKI_data}.csv")
    ))
  } else {
    rep_tri <- read_csv(deconvolved_data_url)
    init_vals <- rep_tri |>
      mutate(value = rowSums(rep_tri[4:ncol(rep_tri)], na.rm = TRUE)) |>
      select(date, location, age_group, value) |>
      filter(
        date >= ymd(start_date_RKI_data) - days(6),
        date < ymd(start_date_RKI_data)
      ) |>
      arrange(-desc(date))
    fs::dir_create(filepath_name)
    write_csv(init_vals, file.path(
      filepath_name,
      glue::glue("initial_values_{start_date_RKI_data}.csv")
    ))
  }

  return(init_vals)
}
