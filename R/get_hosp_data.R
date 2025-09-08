#' Get the hospital admissions data
#'
#' @param location_name Character string indicating long form name of state.
#' @param location_abbr Character string indicating abbreviation of state
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
#' @importFrom dplyr rename mutate filter
#' @importFrom readr read_csv
#' @importFrom lubridate ymd days
get_hosp_data <- function(location_name,
                          location_abbr,
                          forecast_date,
                          right_trunc = FALSE,
                          calibration_period = 100,
                          lag = 3) {
  if (isFALSE(right_trunc)) {
    RKI_hosp_adj <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv") # nolint

    hosp_clean <- RKI_hosp_adj |>
      rename(
        date = Datum,
        state = Bundesland,
        age_group = Altersgruppe,
        # I think this is the nowcasted hospital admissions
        adj_hosp_7d_count = `fixierte_7T_Hospitalisierung_Faelle`,
        # But am not sure
        actual_hosp_7d_count = `aktualisierte_7T_Hospitalisierung_Faelle`,
        state_pop = `Bevoelkerung`
      ) |>
      filter(
        date >= ymd(forecast_date) - days(calibration_period),
        date <= ymd(forecast_date) - days(lag),
        state == location_name
      ) |>
      mutate(state_abbr = location_abbr) |>
      mutate(daily_hosp_admits = adj_hosp_7d_count / 7) |> # hack for now
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
