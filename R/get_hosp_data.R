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

  hosp_clean <- reformat_hosp_data(RKI_hosp_adj,
    location_abbr = location_abbr,
    location_name = location_name,
    forecast_date = forecast_date,
    forecast_horizon = forecast_horizon
  )
  return(hosp_clean)
}

#' Reformat the hospital admissions data (English to German, and daily values)
#'
#' @param RKI_hosp_adj Data.frame of raw data from GitHub
#' @inheritParams get_hosp_for_eval
#' @param col_for_final_counts Character string indicating the column to use
#'   for the "final"counts at each reference date. Default is
#'   `"aktualisierte_7T_Hospitalisierung_Faelle"` which is the updated counts.
#'
#' @returns Data.frame with daily and 7d counts
#' @autoglobal
reformat_hosp_data <- function(RKI_hosp_adj,
                               location_abbr,
                               location_name,
                               forecast_date,
                               forecast_horizon,
                               col_for_final_counts = "aktualisierte_7T_Hospitalisierung_Faelle") { # nolint
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
      # Either the nowcast or the final observed data
      updated_hosp_7d_count = {{ col_for_final_counts }},
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
      date <= ymd(forecast_date) + days(forecast_horizon),
      state == location_name
    ) |>
    select(
      date, state, forecast_date, daily_hosp_admits,
      state_pop,
      updated_hosp_7d_count
    )
  return(hosp_clean)
}

#' Filter hospital admissions data for fitting
#'
#' @param hosp_data_eval hospital admissions data for evaluation step
#' @param hosp_data_real_time Boolean indicating whether to use the hospital
#'   admissions as of the forecast date or just truncate the evaluation data.
#' @inheritParams get_hosp_for_eval
#' @param calibration_period Integer indicating the number of days of
#'    hospital admissions calibration data to extract. Default is `100`.
#'    If NULL, we use all the data.
#' @param lag Integer indicating the number of days from the forecast date of
#'    the latest hospital admission. Default is `3`.
#' @autoglobal
#' @importFrom dplyr filter summarise
#' @importFrom lubridate ymd days
#' @autoglobal
get_hosp_for_fit <- function(hosp_data_eval,
                             location_name,
                             location_abbr,
                             forecast_date,
                             forecast_horizon,
                             hosp_data_real_time,
                             calibration_period = 100,
                             lag = 3) {
  # if hosp_data_real_time is FALSE, truncate the evaluation data
  if (isFALSE(hosp_data_real_time)) {
    if (!is.null(calibration_period)) {
      hosp_for_fit <- hosp_data_eval |>
        filter(
          date >= ymd(forecast_date) - days(calibration_period) - days(lag),
          date <= ymd(forecast_date) - days(lag)
        )
    } else {
      hosp_for_fit <- hosp_data_eval |>
        filter(
          date <= ymd(forecast_date) - days(lag)
        )
    }
    # if hosp_data_real_time is TRUE, then get the data as of the forecast date
  } else {
    hosp_for_fit <- get_hosp_as_of_forecast_date(
      forecast_date = forecast_date,
      location_name = location_name,
      location_abbr = location_abbr,
      forecast_horizon = forecast_horizon
    )
    if (!is.null(calibration_period)) {
      last_data_date <- hosp_for_fit |>
        filter(!is.na(daily_hosp_admits)) |>
        summarise(max_date = max(date)) |>
        pull(max_date)
      hosp_for_fit <- filter(
        hosp_for_fit,
        date >= last_data_date - days(calibration_period),
        !is.na(daily_hosp_admits)
      )
    }
  }

  hosp_for_fit <- mutate(hosp_for_fit,
    hosp_data_real_time = hosp_data_real_time
  )
  return(hosp_for_fit)
}

#' Get hospital admissions data as of the forecast date
#'
#' @inheritParams get_hosp_for_eval
#' @param filepath_name Character string indicating file path to save or
#'   extract hospital admissions vintage
#' @param hosp_data_url Character string of URL to hospital admissions data
#'
#' @importFrom fs dir_create
#' @returns Data.frame of hospital admissions as of the forecast date
get_hosp_as_of_forecast_date <- function(forecast_date,
                                         location_name,
                                         location_abbr,
                                         forecast_horizon,
                                         filepath_name = file.path(
                                           "input",
                                           "data",
                                           "hosp",
                                           "vintages"
                                         ),
                                         hosp_data_url = "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv" # nolint
) {
  vintage_fp <- file.path(filepath_name, forecast_date, "hosp_data_as_of.csv")
  if (file.exists(vintage_fp)) {
    hosp_data <- read_csv(vintage_fp)
  } else {
    # Need to fine the url associated with the commit history as of the
    # forecast date
    hosp_vintage_data_url <- get_vintage(
      raw_url = hosp_data_url,
      target_date = forecast_date
    )
    hosp_data <- read_csv(hosp_vintage_data_url)
    dir_create(file.path(filepath_name, forecast_date),
      recursive = TRUE, showWarnings = FALSE
    )
    write_csv(hosp_data, vintage_fp)
  }

  hosp_data_clean <- reformat_hosp_data(
    RKI_hosp_adj = hosp_data,
    location_abbr = location_abbr,
    location_name = location_name,
    forecast_date = forecast_date,
    forecast_horizon = forecast_horizon,
    col_for_final_counts = "PS_adjustierte_7T_Hospitalisierung_Faelle" # nolint
  )
  return(hosp_data_clean)
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
