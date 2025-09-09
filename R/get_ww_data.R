#' Get wastewater data
#' For now, just pull the latest wastewater data and filter to exclude the
#' most recent dates
#'
#' @inheritParams get_hosp_data
#' @importFrom dplyr mutate filter select rename
#' @importFrom readr read_tsv
#' @autoglobal
get_ww_data <- function(location_name,
                        location_abbr,
                        forecast_date,
                        calibration_period = 100,
                        lag = 3) {
  # For now, just pull the latest and filter to lag days before the forecast
  # date
  
  RKI_ww_sites <- here::here("inst", "RKI_ww_sites.rds")
  
  if (!file.exists(RKI_ww_sites)) {
    RKI_ww_sites <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/refs/heads/main/amelag_einzelstandorte.tsv") # nolint
    saveRDS(RKI_ww_sites, here::here("inst", "RKI_ww_sites.rds"))
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
      date >= ymd(forecast_date) - days(calibration_period),
      date <= ymd(forecast_date) - days(lag),
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
