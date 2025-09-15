#' Get the hospital admissions data
#'
#' @param location_name Character string indicating long form name of state.
#' @param location_abbr Character string indicating abbreviation of state
#' @param forecast_date Character string or date indicating the date of
#'    forecast in YYYY-MM-DD
#' @param filepath_name Name of directory to save the raw input wastewater data.
#' @param right_trunc Boolean indicating whether to use the real-time, right
#'    truncated data or the final corrected data. Default is `FALSE` indicating
#'    we will just use the corrected data.
#' @param calibration_period Integer indicating the number of days of
#'    hospital admissions calibration data to extract. Default is `100`.
#' @param lag Integer indicating the number of days from the forecast date of
#'    the latest hospital admission. Default is `3`.
#' @autoglobal
#' @importFrom dplyr rename mutate filter
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom lubridate ymd days
get_hosp_data <- function(location_name,
                          location_abbr,
                          forecast_date,
                          filepath_name = file.path("input", "data", "hosp"),
                          right_trunc = FALSE,
                          calibration_period = 100,
                          lag = 3) {
  if (isFALSE(right_trunc)) {
    if (file.exists(file.path(filepath_name, "RKI_hosp_adj.csv"))) {
      RKI_hosp_adj <- read_csv(file.path(filepath_name, "RKI_hosp_adj.csv"))
    } else {
      RKI_hosp_adj <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv") # nolint
      df_init <- read.csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/code/resources/initial_values.csv")
      dir_create(filepath_name)
      write_csv(RKI_hosp_adj, file.path(filepath_name, "RKI_hosp_adj.csv"))


      write_csv(df_daily, file.path(filepath_name, "RKI_initial_values.csv"))
    }

    initial_vals <- df_init |>
      filter(
        location == glue::glue("DE-{location_abbr}"), # dif
        age_group == "00+"
      ) |>
      pull(value)

    hosp_clean <- RKI_hosp_adj |>
      rename(
        date = Datum,
        state = Bundesland,
        age_group = Altersgruppe,
        # I think this is the initial hospital admissions
        adj_hosp_7d_count = `fixierte_7T_Hospitalisierung_Faelle`,
        # This is the updated (closer to or basically final)
        actual_hosp_7d_count = `aktualisierte_7T_Hospitalisierung_Faelle`,
        state_pop = `Bevoelkerung`
      ) |>
      filter(
        date >= ymd(forecast_date) - days(calibration_period),
        date <= ymd(forecast_date) - days(lag),
        state == location_name
      ) |>
      mutate(
        state_abbr = location_abbr,
        daily_hosp_admits = convert_rolling_sum_to_incidence(
          rolling_sums = actual_hosp_7d_count,
          k = 7,
          initial_values = initial_vals
        )
      ) |>
      select(
        date, daily_hosp_admits, state_pop, actual_hosp_7d_count,
        adj_hosp_7d_count
      )
  } else {
    # Insert function to use git history to get the data as of the forecast date
    hosp_clean <- NULL
  }

  return(hosp_clean)
}

generate_and_save_daily_data <- function(url_7d_data,
                                         url_initial_values) {
  RKI_hosp_adj <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv") # nolint
  df_init <- read.csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/code/resources/initial_values.csv")
}
