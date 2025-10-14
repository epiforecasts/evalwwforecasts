#' Get wastewater data
#' For now, just pull the latest wastewater data and filter to exclude the
#' most recent dates
#'
#' @inheritParams get_hosp_for_eval
#' @param filepath_name Name of directory to save the raw input wastewater data.
#' @importFrom dplyr mutate filter select rename
#' @importFrom fs dir_create
#' @importFrom readr read_tsv read_csv write_csv
#' @autoglobal
get_ww_for_eval <- function(location_name,
                            location_abbr,
                            forecast_date,
                            forecast_horizon = 28,
                            filepath_name = file.path("input", "data", "ww")) {
  # For now, just pull the latest and filter to lag days before the forecast
  # date

  if (file.exists(file.path(filepath_name, "RKI_ww_sites.csv"))) {
    RKI_ww_sites <- read_csv(file.path(filepath_name, "RKI_ww_sites.csv"))
  } else {
    RKI_ww_sites <- read_tsv("https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/refs/heads/main/amelag_einzelstandorte.tsv") # nolint
    dir_create(filepath_name)
    write_csv(RKI_ww_sites, file.path(filepath_name, "RKI_ww_sites.csv"))
  }


  ww_clean <- RKI_ww_sites |>
    rename(
      location = "standort",
      date = "datum",
      state = "bundesland",
      conc = "viruslast",
      pop_cov = "einwohner",
      change_in_lab_indicator = "laborwechsel",
      normalized = "viruslast_normalisiert",
      pathogen = "typ",
      below_LOD = "unter_bg"
    ) |>
    select(
      location, date, state, conc, pop_cov, change_in_lab_indicator,
      normalized, pathogen,
      below_LOD
    ) |>
    filter(
      state == location_abbr,
      pathogen == "SARS-CoV-2"
    ) |>
    mutate(
      lab = change_in_lab_indicator,
      log_genome_copies_per_ml = log((conc / 1e3) + 1e-8),
      log_lod = 3 # make this up for now (maybe )
    ) |>
    rename(
      site = location,
      site_pop = pop_cov
    ) |>
    select(
      date, site, lab, log_genome_copies_per_ml, log_lod, site_pop,
      below_LOD
    ) |>
    filter(!is.na(log_genome_copies_per_ml))

  return(ww_clean)
}

#' Filter wastewater data for fitting
#'
#' @param ww_data_eval wastewater data for evaluation step
#' @param forecast_date Character string or date indicating the date of
#'    forecast in YYYY-MM-DD
#' @param calibration_period Integer indicating the number of days of
#'    wastewater calibration data to extract. Default is `100`.
#' @param lag Integer indicating the number of days from the forecast date of
#'    the latest wastewater data. Default is `3`
#' @autoglobal
#' @importFrom dplyr filter
#' @importFrom lubridate ymd days
get_ww_for_fit <- function(ww_data_eval,
                           forecast_date,
                           calibration_period = 100,
                           lag = 3) {
  ww_for_fit <- ww_data_eval |>
    filter(
      date >= ymd(forecast_date) - days(calibration_period),
      date <= ymd(forecast_date) - days(lag)
    ) |>
    mutate(
      forecast_date = forecast_date
    )
  return(ww_for_fit)
}
