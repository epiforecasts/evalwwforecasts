#' Calculate wastewater metadata table
#'
#' Creates a summary table of wastewater metadata aggregated by forecast date
#' and location, including site counts, population coverage, sampling frequency,
#' latency, lab changes, and data variability metrics.
#'
#' @param ww_data Data.frame containing wastewater data with at least the
#'   following columns: forecast_date, location_abbr, location_name, site, lab,
#'   date, log_genome_copies_per_ml, site_pop
#' @param state_pop_data Optional data.frame with columns location_name and
#'   state_pop. If provided, population coverage will be calculated as
#'   proportion of state population. If NULL, total site population will be
#'   reported.
#' @param sampling_freq_window Number of days prior to forecast date to use for
#'   calculating sampling frequency. Default is 90 days.
#'
#' @returns A data.frame with one row per forecast_date and location
#'  combination, containing the following metrics:
#'   - forecast_date: Date of forecast
#'   - location_abbr: State abbreviation
#'   - location_name: State name
#'   - n_sites: Number of wastewater treatment plants
#'   - pop_coverage: Proportion of state population served
#'   (or total if state_pop not provided)
#'   - avg_sampling_freq: Average sampling frequency across sites (observations
#'    per day over prior 90 days)
#'   - max_sampling_freq: Maximum site-level sampling frequency
#'   - avg_latency: Mean days from last collection to forecast date
#'   - min_latency: Minimum latency across sites
#'   - avg_lab_changes: Average number of laboratory transitions per site
#'   - min_data_variability: Minimum coefficient of variation of log genome
#'    copies
#'   - avg_data_variability: Average coefficient of variation across all sites
#'   - prop_below_LOD: proportion of observations below LOD across all sites
#'
#' @autoglobal
#' @importFrom dplyr group_by summarise n_distinct n mutate ungroup arrange
#' left_join filter first
#' @importFrom lubridate ymd days
#' @export
calculate_ww_metadata_table <- function(ww_data,
                                        state_pop_data = NULL,
                                        sampling_freq_window = 90) {
  # Filter data to sampling frequency window and calculate site-level metrics
  site_metrics <- ww_data |>
    group_by(forecast_date, location_abbr, location_name) |>
    # Filter to sampling frequency window
    filter(date >= ymd(forecast_date) - days(sampling_freq_window)) |>
    ungroup() |>
    group_by(forecast_date, location_abbr, location_name, site) |>
    summarise(
      # Site population (take unique value)
      site_pop = first(site_pop),
      # Number of observations for this site in the window
      n_obs = n(),
      # Date range for this site
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE),
      # Days covered in the sampling frequency window
      n_days_in_window = as.numeric(
        min(max_date, ymd(first(forecast_date))) -
          max(min_date, ymd(first(forecast_date)) -
            days(sampling_freq_window)) + 1
      ),
      # Sampling frequency (observations per day over the window)
      sampling_freq = n_obs / pmax(n_days_in_window, 1),
      sampling_freq_overall = n_obs / as.numeric(
        max_date - min_date
      ),
      # Latency (days from last observation to forecast date)
      latency = as.numeric(ymd(first(forecast_date)) - max_date),
      # Number of lab changes (distinct labs - 1)
      n_labs = n_distinct(lab),
      lab_changes = n_labs - 1,
      data_variability = {
        values <- log_genome_copies_per_ml[!is.na(log_genome_copies_per_ml)]
        if (length(values) > 1 && mean(values) != 0) {
          sd(values) / abs(mean(values))
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )

  # Aggregate to forecast_date and location level
  metadata_table <- site_metrics |>
    group_by(forecast_date, location_abbr, location_name) |>
    summarise(
      # 1. Site count
      n_sites = n_distinct(site),

      # 2. Total population coverage (sum of all site populations)
      total_site_pop = sum(site_pop, na.rm = TRUE),

      # 3. Average sampling frequency across sites
      avg_sampling_freq = mean(sampling_freq_overall, na.rm = TRUE),

      # 4. Maximum sampling frequency overall
      max_sampling_freq = max(sampling_freq_overall, na.rm = TRUE),

      # 5. Average latency across sites
      avg_latency = mean(latency, na.rm = TRUE),

      # 6. Minimum latency
      min_latency = min(latency, na.rm = TRUE),

      # 7. Average lab changes per site
      avg_lab_changes = mean(lab_changes, na.rm = TRUE),

      # 8. Minimum data variability
      min_data_variability = min(data_variability, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate variability and proportion below LOD across the state
  lod_data <- ww_data |>
    group_by(forecast_date, location_abbr, location_name) |>
    # Filter to same sampling frequency window for consistency
    filter(date >= ymd(forecast_date) - days(sampling_freq_window)) |>
    summarise(
      avg_data_variability = {
        values <- log_genome_copies_per_ml[!is.na(log_genome_copies_per_ml)]
        if (length(values) > 1 && mean(values) != 0) {
          sd(values) / abs(mean(values))
        } else {
          NA_real_
        }
      },
      # Proportion below the LOD
      prop_below_LOD = sum(below_LOD == "ja") / n(),
      .groups = "drop"
    )

  # Join CV data to metadata table
  metadata_table <- metadata_table |>
    left_join(lod_data, by = c(
      "forecast_date",
      "location_abbr",
      "location_name"
    ))

  # If state population data is provided, calculate true population coverage
  if (!is.null(state_pop_data)) {
    metadata_table <- metadata_table |>
      left_join(state_pop_data, by = "location_name") |>
      mutate(pop_coverage = total_site_pop / state_pop)
  } else {
    # Otherwise, report as total site population
    metadata_table <- rename(metadata_table,
      pop_coverage = total_site_pop
    )
  }

  metadata_table <- arrange(
    metadata_table,
    forecast_date, location_abbr
  )

  return(metadata_table)
}

#' Get a table of state names and population sizes
#'
#' @param hosp_url Hospital admissions data url
#' @param filepath_name Directory to save states and population sizes
#'
#' @returns Data.frame of states and population sizes
get_state_pop_data <- function(hosp_url = "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv", # nolint
                               filepath_name = file.path("metadata")) {
  if (file.exists(file.path(filepath_name, "state_pop_data.csv"))) {
    state_pop_data <- read_csv(file.path(filepath_name, "state_pop_data.csv"))
  } else {
    state_pop_data <- read_csv(hosp_url) |>
      rename(
        location_name = Bundesland,
        state_pop = `Bevoelkerung`
      ) |>
      group_by(location_name) |>
      summarise(state_pop = max(state_pop))

    dir_create(filepath_name)
    write_csv(state_pop_data, file.path(filepath_name, "state_pop_data.csv"))
  }
  return(state_pop_data)
}
