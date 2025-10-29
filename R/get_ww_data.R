#' Get wastewater data
#' For now, just pull the latest wastewater data and filter to exclude the
#' most recent dates
#'
#' @inheritParams get_hosp_for_eval
#' @param filepath_name Name of directory to save the raw input wastewater data.
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


  # Rename columns and format for wwinference
  ww_clean <- reformat_ww_data(
    raw_ww = RKI_ww_sites,
    location_name = location_name,
    location_abbr = location_abbr
  )

  return(ww_clean)
}

#' Use the Git commit history to get the wastewater data available as of
#' the forecast date. Subsequent function will filter it to the location we
#' want.
#'
#' @inheritParams get_hosp_for_eval
#' @inheritParams get_ww_for_eval
#' @param forecast_date Character string or date indicating the date of
#'    forecast in YYYY-MM-DD
#' @param calibration_period Integer indicating the number of days of
#'    wastewater calibration data to extract. Default is `100`.
#' @param ww_data_url Character string of the url of the wastewater data (not
#'   the raw version).

#' @autoglobal
#' @importFrom dplyr filter
#' @importFrom lubridate ymd days
get_ww_as_of_forecast_date <- function(forecast_date,
                                       location_name,
                                       location_abbr,
                                       filepath_name = file.path("input", "data", "ww", "vintages"), # nolint
                                       calibration_period = 100,
                                       ww_data_url = "https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/refs/heads/main/amelag_einzelstandorte.tsv") { # nolint
  vintage_fp <- file.path(filepath_name, forecast_date, "ww_sites_as_of.csv")
  if (file.exists(vintage_fp)) {
    RKI_ww_sites <- read_csv(vintage_fp)
  } else {
    # Need to fine the url associated with the commit history as of the
    # forecast date
    ww_vintage_data_url <- get_vintage(
      raw_url = ww_data_url,
      target_date = forecast_date
    )
    RKI_ww_sites <- read_tsv(ww_vintage_data_url)
    dir_create(file.path(filepath_name, forecast_date))
    write_csv(RKI_ww_sites, vintage_fp)
  }

  # Clean the and filter to the calibration period, add the forecast date
  ww_for_fit <- reformat_ww_data(
    raw_ww = RKI_ww_sites,
    location_name = location_name,
    location_abbr = location_abbr
  ) |>
    filter(
      date >= ymd(forecast_date) - days(calibration_period)
    ) |>
    mutate(forecast_date = forecast_date)

  return(ww_for_fit)
}


#' Use GitHub raw url and commit history to get the url as of a specific target
#'   date.
#'
#' @param raw_url Character string indicating url of latest data.
#' @param target_date As of date
#' @param github_token Optional Personal Access Token
#'
#' @returns Character string of the url to use to get the data as of a certain
#'   date.
#' @autoglobal
#' @importFrom httr parse_url GET stop_for_status
#' @importFrom jsonlite fromJSON
get_vintage <- function(raw_url,
                        target_date,
                        github_token = NULL) {
  parsed <- parse_url(raw_url)
  path_parts <- strsplit(gsub("^/|/$", "", parsed$path), "/")[[1]] # nolint

  owner <- path_parts[1]
  repo <- path_parts[2]

  if (length(path_parts) > 3 && path_parts[3] == "refs" && path_parts[4] == "heads") { # nolint
    branch <- path_parts[5]
    file_path <- paste(path_parts[6:length(path_parts)], collapse = "/")
  } else {
    branch <- path_parts[3]
    file_path <- paste(path_parts[4:length(path_parts)], collapse = "/")
  }

  target_datetime <- as.POSIXct(paste0(target_date, " 23:59:59"), tz = "UTC")
  target_iso <- format(target_datetime, "%Y-%m-%dT%H:%M:%SZ")

  headers <- c("Accept" = "application/vnd.github.v3+json") # nolint
  if (!is.null(github_token)) {
    headers <- c(headers, "Authorization" = paste("token", github_token)) # nolint
  }

  api_url <- sprintf("https://api.github.com/repos/%s/%s/commits", owner, repo)

  response <- GET(
    api_url,
    httr::add_headers(.headers = headers),
    query = list(
      path = file_path,
      sha = branch,
      until = target_iso,
      per_page = 1
    )
  )

  stop_for_status(response)

  commits <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  if (length(commits) == 0 || nrow(commits) == 0) {
    stop(sprintf("No commits found for %s before %s", file_path, target_date),
      call. = FALSE
    )
  }
  # Get the commit SHA
  commit_sha <- commits$sha[1]
  commit_date <- commits$commit$committer$date[1]

  # Construct the historical raw URL
  historical_url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s/%s",
    owner, repo, commit_sha, file_path
  )

  message(sprintf(
    "Found commit %s from %s",
    substr(commit_sha, 1, 7), commit_date
  ))

  return(historical_url)
}

#' Reformat the ww data, selecting only the location we need and the variables
#'  we need.
#'
#' @param raw_ww Data.frame from the RKI GitHub
#' @inheritParams get_hosp_for_eval
#' @param log_lod_val Scalar indicating the value to reset the
#'    limit of detection (LOD) to, to be
#'   removed in future iterations and replace with an LOD value at each
#'   measurement or site.
#'
#' @returns Data.frame of cleaned wastewater values from location_abbr.
#' @importFrom dplyr mutate filter select rename
#' @autoglobal
reformat_ww_data <- function(raw_ww,
                             location_abbr,
                             location_name,
                             log_lod_val = 1) {
  if ("unter_bg" %in% names(raw_ww)) {
    raw_ww <- dplyr::rename(raw_ww, below_LOD = unter_bg)
  } else if ("unter_BG" %in% names(raw_ww)) {
    raw_ww <- dplyr::rename(raw_ww, below_LOD = unter_BG)
  } else { # Assume its no if not present in the data?
    raw_ww <- dplyr::mutate(raw_ww, below_LOD = "nein")
  }
  if ("typ" %in% names(raw_ww)) {
    raw_ww <- raw_ww |>
      dplyr::rename(pathogen = "typ") |>
      dplyr::filter(pathogen == "SARS-CoV-2")
  } else {
    raw_ww <- dplyr::mutate(
      raw_ww,
      pathogen = "SARS-CoV-2"
    )
  }
  if ("laborwechsel" %in% names(raw_ww)) {
    raw_ww <- dplyr::rename(raw_ww,
      change_in_lab_indicator = "laborwechsel"
    )
  } else {
    raw_ww <- dplyr::mutate(raw_ww,
      change_in_lab_indicator = 1
    )
  }

  ww_clean <- raw_ww |>
    rename(
      location = "standort",
      date = "datum",
      state = "bundesland",
      conc = "viruslast",
      pop_cov = "einwohner"
    ) |>
    select(
      location, date, state, conc, pop_cov, change_in_lab_indicator, pathogen,
      below_LOD
    ) |>
    filter(
      state == location_abbr
    ) |>
    mutate(
      lab = glue::glue("{location}-{change_in_lab_indicator}"),
      log_genome_copies_per_ml = log((conc / 1e3) + 1e-8),
      log_lod = log_lod_val, # make this up for now (maybe )
      location_name = location_name,
      location_abbr = location_abbr
    ) |>
    rename(
      site = location,
      site_pop = pop_cov
    ) |>
    select(
      date, site, lab, log_genome_copies_per_ml, log_lod, site_pop,
      below_LOD, location_abbr, location_name
    ) |>
    filter(!is.na(log_genome_copies_per_ml))

  ww_w_lod <- add_correct_lod(ww_clean)
  return(ww_w_lod)
}

#' Add the correct LOD using external data on LOQ for each gene at each site
#'   and time point
#'
#' @param ww_data Data.frame of wastewater data from an individual location
#' @param path_to_lod_vals Character string indicating the file path to the LOQ
#'   data
#'
#' @returns Data.frame containing updated `ww_data` where the LOD column has
#'   now been filled in with the geometric mean of the LOQ across all genes
#'   measured on that date and in that site, if the data is flagged as being
#'   below the LOD.
add_correct_lod <- function(ww_data,
                            path_to_lod_vals = file.path(
                              "input", "data",
                              "loq_data.csv"
                            )) {
  lod_vals <- read_csv(path_to_lod_vals)
  overall_mean_loq <- lod_vals |>
    group_by(Standort, Bundesland, date) |>
    summarise(mean_loq = exp(mean(log(loq)))) |>
    ungroup() |>
    summarise(overall_mean = mean(mean_loq, na.rm = TRUE)) |>
    pull(overall_mean)
  lod_vals_clean <- lod_vals |>
    mutate(
      Standort =
        case_when(
          Standort == "Düsseldorf_Nord" ~ "Düsseldorf (Nord)",
          Standort == "Düsseldorf_Süd" ~ "Düsseldorf (Süd)",
          Standort == "Frankfurt" ~ "Frankfurt (Oder)",
          Standort == "Halle/Saale" ~ "Halle (Saale)",
          Standort == "Hamburg Nord" ~ "Hamburg 01",
          Standort == "Hamburg Süd" ~ "Hamburg 02",
          Standort == "Primasens-Felsalbe" ~ "Pirmasens-Felsalbe",
          TRUE ~ Standort
        )
    ) |>
    filter(Standort %in% c(unique(ww_data$site)))

  mean_lod <- lod_vals_clean |>
    filter(!is.na(loq)) |>
    group_by(Standort, Bundesland, date) |>
    summarise(mean_loq = exp(mean(log(loq), na.rm = TRUE)))

  ww_data_lod_joined <- ww_data |>
    left_join(mean_lod, by = c(
      "site" = "Standort",
      "location_name" = "Bundesland",
      "date"
    )) |>
    mutate(log_lod = case_when(
      !is.na(mean_loq) & below_LOD == "ja" ~ log(mean_loq),
      is.na(mean_loq) & below_LOD == "ja" ~ log(overall_mean_loq),
      TRUE ~ log_lod
    )) |>
    select(-mean_loq)
  return(ww_data_lod_joined)
}
