#' Load hospital forecast data for multiple dates and locations
#'
#' @param output_path Path to the output folder
#' @param forecast_dates Character vector of forecast dates
#' @param locations Character vector of location names
#' @return List of hospital forecast data frames
#' @importFrom readr read_csv
#' @export
load_hospital_forecasts <- function(output_path, forecast_dates, locations) {
  hosp_forecasts_list <- list()
  for (forecast_date in forecast_dates) {
    forecast_path <- file.path(
      output_path, "individual_forecasts_all_runs", forecast_date
    )

    for (loc in locations) {
      data_dir <- file.path(forecast_path, loc, "data")
      file_specs <- list(
        list(file = "hosp_quantiles_ww_TRUE.csv", suffix = "_ww_TRUE"),
        list(file = "hosp_quantiles_ww_FALSE.csv", suffix = "_ww_FALSE"),
        list(file = "hosp_quantiles_arima.csv", suffix = "_arima")
      )
      for (spec in file_specs) {
        fpath <- file.path(data_dir, spec$file)
        if (file.exists(fpath)) {
          temp_data <- read_csv(fpath, show_col_types = FALSE)
          temp_data$forecast_date_chr <- forecast_date
          key <- paste0(forecast_date, "_", loc, spec$suffix)
          hosp_forecasts_list[[key]] <- temp_data
        }
      }
    }
  }
  return(hosp_forecasts_list)
}

#' Load wastewater forecast data for multiple dates and locations
#'
#' @param output_path Path to the output folder
#' @param forecast_dates Character vector of forecast dates
#' @param locations Character vector of location names
#' @return List of wastewater forecast data frames
#' @importFrom readr read_csv
#' @export
load_ww_forecasts <- function(output_path, forecast_dates, locations) {
  ww_forecasts_list <- list()
  for (forecast_date in forecast_dates) {
    forecast_path <- file.path(
      output_path, "individual_forecasts_all_runs", forecast_date
    )
    for (loc in locations) {
      ww_path <- file.path(
        forecast_path, loc, "data", "ww_quantiles.csv"
      )
      if (file.exists(ww_path)) {
        ww_data <- read_csv(ww_path, show_col_types = FALSE)
        ww_data$location <- loc
        ww_data$forecast_date_chr <- forecast_date
        ww_forecasts_list[[paste0(forecast_date, "_", loc)]] <- ww_data
      }
    }
  }
  return(ww_forecasts_list)
}

#' Load later wastewater observations for forecast validation
#'
#' @param output_path Path to the output folder
#' @param forecast_dates Character vector of forecast dates
#' @param locations Character vector of location names
#' @return List of later wastewater observation data frames
#' @importFrom readr read_csv
#' @importFrom lubridate ymd days
#' @export
load_later_ww_obs <- function(output_path, forecast_dates, locations) {
  ww_later_obs_list <- list()

  all_forecast_dirs <- list.dirs(
    file.path(output_path, "individual_forecasts_all_runs"),
    full.names = FALSE,
    recursive = FALSE
  )
  all_forecast_dates <- sort(
    grep("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", all_forecast_dirs, value = TRUE)
  )

  max_current_forecast <- max(forecast_dates)
  target_date <- ymd(max_current_forecast) + days(56)

  all_forecast_dates_parsed <- ymd(all_forecast_dates)
  later_dates <- all_forecast_dates_parsed[
    all_forecast_dates_parsed >= target_date
  ]

  if (length(later_dates) > 0) {
    later_forecast_date <- as.character(min(later_dates))

    later_path <- file.path(
      output_path, "individual_forecasts_all_runs", later_forecast_date
    )
    for (loc in locations) {
      ww_path <- file.path(later_path, loc, "data", "ww_quantiles.csv")
      if (file.exists(ww_path)) {
        ww_later_data <- read_csv(ww_path, show_col_types = FALSE)
        ww_later_data$location <- loc
        ww_later_obs_list[[loc]] <- ww_later_data
      }
    }
  }

  return(ww_later_obs_list)
}

#' Process hospital forecast data
#'
#' @param hosp_forecasts Combined hospital forecasts data frame
#' @param forecast_horizon_to_plot Forecast horizon in days
#' @param historical_data_to_plot Historical data period in days
#' @param scale_selected Scale to use
#' @return List with processed hospital forecasts, fits, and observations
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate ymd days
#' @export
process_hospital_data <- function(hosp_forecasts,
                                  forecast_horizon_to_plot,
                                  historical_data_to_plot,
                                  scale_selected) {
  hosp_forecasts$date_parsed <- ymd(hosp_forecasts$date)
  hosp_forecasts$forecast_date_parsed <- ymd(hosp_forecasts$forecast_date_chr)

  # Filter for forecasts (dates >= forecast_date)
  forecasts_filtered <- hosp_forecasts[
    hosp_forecasts$date_parsed >= hosp_forecasts$forecast_date_parsed &
      hosp_forecasts$date_parsed <= (
        hosp_forecasts$forecast_date_parsed +
          days(forecast_horizon_to_plot - 1)
      ) &
      hosp_forecasts$scale == scale_selected,
  ]

  forecasts_filtered$model_ww <- dplyr::case_when(
    forecasts_filtered$model == "arima_baseline" ~ "arima_baseline",
    forecasts_filtered$include_ww ~ "wwinference-TRUE",
    TRUE ~ "wwinference-FALSE"
  )
  forecasts_filtered$forecast_date_model_ww <- paste0(
    forecasts_filtered$forecast_date_chr, "-", forecasts_filtered$model_ww
  )

  forecasts_wide <- pivot_quantiles(forecasts_filtered)

  min_forecast_date <- min(forecasts_wide$forecast_date_parsed, na.rm = TRUE)
  max_forecast_date <- max(forecasts_wide$forecast_date_parsed, na.rm = TRUE)
  min_date_filter <- min_forecast_date - days(historical_data_to_plot)
  max_date_filter <- max_forecast_date + days(forecast_horizon_to_plot - 1)

  # Filter for historical fits (dates < forecast_date)
  fits_filtered <- hosp_forecasts[
    hosp_forecasts$date_parsed < hosp_forecasts$forecast_date_parsed &
      hosp_forecasts$date_parsed >= min_date_filter &
      hosp_forecasts$scale == scale_selected,
  ]

  fits_filtered$model_ww <- dplyr::case_when(
    fits_filtered$model == "arima_baseline" ~ "arima_baseline",
    fits_filtered$include_ww ~ "wwinference-TRUE",
    TRUE ~ "wwinference-FALSE"
  )
  fits_filtered$forecast_date_model_ww <- paste0(
    fits_filtered$forecast_date_chr, "-", fits_filtered$model_ww
  )

  fits_wide <- pivot_quantiles(fits_filtered)

  hosp_obs <- hosp_forecasts[
    hosp_forecasts$date_parsed >= min_date_filter &
      hosp_forecasts$date_parsed <= max_date_filter &
      !is.na(hosp_forecasts$observed),
    c("date_parsed", "location", "observed", "forecast_date_parsed")
  ]

  hosp_obs <- hosp_obs[
    !duplicated(hosp_obs[, c("date_parsed", "location")]),
  ]

  hosp_obs$obs_timing <- ifelse(
    hosp_obs$date_parsed < min_forecast_date,
    "historical",
    "future"
  )

  forecasts_wide$facet_col <- "Hospital Admissions"
  forecasts_wide$data_type <- "forecast"
  fits_wide$facet_col <- "Hospital Admissions"
  fits_wide$data_type <- "fit"
  hosp_obs$facet_col <- "Hospital Admissions"
  hosp_obs$data_type <- "observed"

  return(list(
    forecasts = forecasts_wide,
    fits = fits_wide,
    observations = hosp_obs,
    min_forecast_date = min_forecast_date,
    min_date_filter = min_date_filter,
    max_date_filter = max_date_filter
  ))
}

#' Process wastewater forecast data
#'
#' @param ww_forecasts Combined wastewater forecasts
#' @param ww_later_obs_list List of later observations
#' @param forecast_horizon_to_plot Forecast horizon in days
#' @param min_date_filter Minimum date for filtering
#' @param max_date_filter Maximum date for filtering
#' @param min_forecast_date Minimum forecast date
#' @return List with processed wastewater forecasts, fits, and observations
#' @importFrom dplyr filter bind_rows mutate rename select
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate ymd days
#' @export
process_ww_data <- function(ww_forecasts,
                            ww_later_obs_list,
                            forecast_horizon_to_plot,
                            min_date_filter,
                            max_date_filter,
                            min_forecast_date) {
  ww_forecasts$date_parsed <- ymd(ww_forecasts$date)
  ww_forecasts$forecast_date_parsed <- ymd(ww_forecasts$forecast_date_chr)

  # Filter for forecasts (dates >= forecast_date)
  ww_filtered <- ww_forecasts[
    ww_forecasts$date_parsed >= ww_forecasts$forecast_date_parsed &
      ww_forecasts$date_parsed <= (
        ww_forecasts$forecast_date_parsed +
          days(forecast_horizon_to_plot - 1)
      ),
  ]

  ww_wide <- pivot_quantiles(ww_filtered)

  ww_wide$facet_col <- ww_wide$lab_site_name
  ww_wide$forecast_date_site <- paste0(
    ww_wide$forecast_date_chr, "-", ww_wide$lab_site_name
  )
  ww_wide$data_type <- "forecast"

  # Filter for historical fits (dates < forecast_date)
  ww_fits_filtered <- ww_forecasts[
    ww_forecasts$date_parsed < ww_forecasts$forecast_date_parsed &
      ww_forecasts$date_parsed >= min_date_filter,
  ]

  ww_fits_wide <- pivot_quantiles(ww_fits_filtered)

  ww_fits_wide$facet_col <- ww_fits_wide$lab_site_name
  ww_fits_wide$forecast_date_site <- paste0(
    ww_fits_wide$forecast_date_chr, "-", ww_fits_wide$lab_site_name
  )
  ww_fits_wide$data_type <- "fit"

  # Get observations that were available at forecast time - these are always historical
  ww_obs_all <- ww_forecasts[
    ww_forecasts$date_parsed >= min_date_filter &
      ww_forecasts$date_parsed <= max_date_filter &
      !is.na(ww_forecasts$log_genome_copies_per_ml),
    c(
      "date_parsed", "location", "site", "lab", "lab_site_name",
      "log_genome_copies_per_ml"
    )
  ]
  ww_obs_all$obs_timing <- "historical"

  if (length(ww_later_obs_list) > 0) {
    # Get observations from later dataset to identify additional observations
    # (either truly future or retrospectively available)
    ww_later_combined <- bind_rows(ww_later_obs_list)
    ww_later_combined$date_parsed <- ymd(ww_later_combined$date)

    ww_later_obs <- ww_later_combined[
      ww_later_combined$date_parsed >= min_forecast_date &
        ww_later_combined$date_parsed <= max_date_filter &
        !is.na(ww_later_combined$log_genome_copies_per_ml),
      c(
        "date_parsed", "location", "site",
        "log_genome_copies_per_ml"
      )
    ]

    site_mapping <- unique(ww_obs_all[, c("site", "location", "lab_site_name")])
    ww_later_obs <- merge(
      ww_later_obs,
      site_mapping,
      by = c("site", "location"),
      all.x = TRUE
    )

    ww_later_obs <- ww_later_obs[!is.na(ww_later_obs$lab_site_name), ]
    ww_later_obs$obs_timing <- "future"

    # Add the future observations to the existing historical ones
    ww_obs_all <- bind_rows(ww_obs_all, ww_later_obs)
    ww_obs_all <- ww_obs_all[
      !duplicated(ww_obs_all[, c("date_parsed", "location", "site")]),
    ]
  }

  ww_obs_all$facet_col <- ww_obs_all$lab_site_name
  ww_obs_all$data_type <- "observed"
  ww_obs <- ww_obs_all[
    !duplicated(
      ww_obs_all[, c("date_parsed", "location", "lab_site_name")]
    ),
  ]

  return(list(
    forecasts = ww_wide,
    fits = ww_fits_wide,
    observations = ww_obs
  ))
}

#' Create hospital plot for a single location
#'
#' @param loc_hosp_forecast Hospital forecast data for location
#' @param loc_hosp_fit Hospital fit data for location
#' @param loc_hosp_obs Hospital observations for location
#' @param loc Location name
#' @param locations All locations (for indexing)
#' @param hosp_ylab Y-axis label
#' @return ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point geom_vline
#'   theme element_text element_blank labs ggtitle ylab scale_shape_manual
#'   scale_fill_manual scale_alpha_manual guide_legend unit
#' @importFrom ggnewscale new_scale_fill
#' @export
#' @autoglobal
create_hospital_plot <- function(loc_hosp_forecast,
                                 loc_hosp_fit,
                                 loc_hosp_obs,
                                 loc,
                                 locations,
                                 hosp_ylab) {
  # Get unique forecast dates for vertical lines
  forecast_dates <- unique(loc_hosp_forecast$forecast_date_parsed)

  p_hosp <- ggplot() +
    forecast_ribbon_layers(loc_hosp_fit) +
    forecast_ribbon_layers(loc_hosp_forecast) +
    geom_vline(
      xintercept = forecast_dates,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.5
    ) +
    model_ww_color_scales() +
    new_scale_fill() +
    geom_point(
      data = loc_hosp_obs,
      aes(x = date_parsed, y = observed, shape = obs_timing, fill = obs_timing),
      color = "black",
      size = 2
    ) +
    scale_shape_manual(
      name = "Observations",
      values = c("historical" = 21, "future" = 21),
      labels = c(
        "historical" = "Available at forecast date",
        "future" = "Available retrospectively"
      ),
      guide = guide_legend(order = 2)
    ) +
    scale_fill_manual(
      name = "Observations",
      values = c("historical" = "black", "future" = "white"),
      labels = c(
        "historical" = "Available at forecast date",
        "future" = "Available retrospectively"
      ),
      guide = guide_legend(order = 2)
    ) +
    lshtm_theme() +
    ylab(hosp_ylab) +
    ggtitle(loc) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 0.5)
    )

  return(p_hosp)
}

#' Determine y-axis labels for middle location
#'
#' @param loc Current location name
#' @param locations Vector of all location names
#' @return List with hospital and wastewater y-axis labels
#' @keywords internal
get_yaxis_labels <- function(loc, locations) {
  middle_loc_index <- ceiling(length(locations) / 2)
  is_middle <- which(locations == loc) == middle_loc_index

  return(list(
    hosp = if (is_middle) "7-day rolling sum of\nhospital admissions" else "",
    ww = if (is_middle) "Log genome copies per mL" else ""
  ))
}

#' Create wastewater plot for a single location
#'
#' @param loc_ww_forecast Wastewater forecast data for location
#' @param loc_ww_fit Wastewater fit data for location
#' @param loc_ww_obs Wastewater observations for location
#' @param ww_ylab Y-axis label
#' @param n_ww_sites_loc Number of WW sites for this location
#' @param max_ww_sites Maximum WW sites across all locations
#' @return ggplot or patchwork object
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point geom_vline
#'   facet_wrap theme element_text element_blank ggtitle ylab scale_shape_manual
#'   scale_fill_manual guide_legend
#' @importFrom patchwork plot_spacer wrap_plots
#' @export
#' @autoglobal
create_ww_plot <- function(loc_ww_forecast,
                           loc_ww_fit,
                           loc_ww_obs,
                           ww_ylab,
                           n_ww_sites_loc,
                           max_ww_sites) {
  # Select sites with the most observations
  site_counts <- table(loc_ww_obs$facet_col)
  top_sites <- names(sort(site_counts, decreasing = TRUE))
  top_facet_cols <- head(top_sites, n_ww_sites_loc)

  # Get unique forecast dates for vertical lines
  forecast_dates <- unique(loc_ww_forecast$forecast_date_parsed)

  # Filter observations for top facets
  loc_ww_obs_filtered <- loc_ww_obs[loc_ww_obs$facet_col %in% top_facet_cols, ]

  p_ww <- ggplot() +
    geom_line(
      data = loc_ww_fit[loc_ww_fit$facet_col %in% top_facet_cols, ],
      aes(x = date_parsed, y = q_0.5, group = forecast_date_site),
      color = "#01454F"
    ) +
    geom_ribbon(
      data = loc_ww_fit[loc_ww_fit$facet_col %in% top_facet_cols, ],
      aes(
        x = date_parsed,
        ymin = q_0.25,
        ymax = q_0.75,
        group = forecast_date_site
      ),
      alpha = 0.4,
      fill = "#01454F"
    ) +
    geom_ribbon(
      data = loc_ww_fit[loc_ww_fit$facet_col %in% top_facet_cols, ],
      aes(
        x = date_parsed,
        ymin = q_0.025,
        ymax = q_0.975,
        group = forecast_date_site
      ),
      alpha = 0.3,
      fill = "#01454F"
    ) +
    geom_line(
      data = loc_ww_forecast[loc_ww_forecast$facet_col %in% top_facet_cols, ],
      aes(x = date_parsed, y = q_0.5, group = forecast_date_site),
      color = "#01454F"
    ) +
    geom_ribbon(
      data = loc_ww_forecast[loc_ww_forecast$facet_col %in% top_facet_cols, ],
      aes(
        x = date_parsed,
        ymin = q_0.25,
        ymax = q_0.75,
        group = forecast_date_site
      ),
      alpha = 0.4,
      fill = "#01454F"
    ) +
    geom_ribbon(
      data = loc_ww_forecast[loc_ww_forecast$facet_col %in% top_facet_cols, ],
      aes(
        x = date_parsed,
        ymin = q_0.025,
        ymax = q_0.975,
        group = forecast_date_site
      ),
      alpha = 0.3,
      fill = "#01454F"
    ) +
    geom_vline(
      xintercept = forecast_dates,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.5
    ) +
    geom_point(
      data = loc_ww_obs_filtered,
      aes(
        x = date_parsed, y = log_genome_copies_per_ml,
        shape = obs_timing, fill = obs_timing
      ),
      color = "black",
      size = 1.5,
      show.legend = FALSE
    ) +
    scale_shape_manual(
      values = c("historical" = 21, "future" = 21),
      guide = "none"
    ) +
    scale_fill_manual(
      values = c("historical" = "black", "future" = "white"),
      guide = "none"
    ) +
    facet_wrap(~facet_col, scales = "free_y", nrow = 1) +
    lshtm_theme() +
    ylab(ww_ylab) +
    ggtitle("") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 0.5)
    )


  return(p_ww)
}

#' Create combined plot for a single location
#'
#' @param loc Location name
#' @param forecasts_wide Hospital forecast data
#' @param fits_wide Hospital fit data
#' @param hosp_obs Hospital observations
#' @param ww_wide Wastewater forecast data (can be NULL)
#' @param ww_fits_wide Wastewater fit data (can be NULL)
#' @param ww_obs Wastewater observations (can be NULL)
#' @param locations Vector of all location names
#' @param max_ww_sites Maximum WW sites across all locations
#' @return A combined plot for the location
#' @keywords internal
create_location_plot <- function(loc, forecasts_wide, fits_wide, hosp_obs,
                                 ww_wide, ww_fits_wide, ww_obs, locations, max_ww_sites) {
  # Filter data for this location
  loc_hosp_forecast <- forecasts_wide[forecasts_wide$location == loc, ]
  loc_hosp_fit <- fits_wide[fits_wide$location == loc, ]
  loc_hosp_obs <- hosp_obs[hosp_obs$location == loc, ]

  # Get y-axis labels
  ylabs <- get_yaxis_labels(loc, locations)

  # Create hospital plot
  p_hosp <- create_hospital_plot(
    loc_hosp_forecast, loc_hosp_fit, loc_hosp_obs, loc, locations, ylabs$hosp
  )

  # Create wastewater plot if data available
  if (!is.null(ww_wide)) {
    loc_ww_forecast <- ww_wide[ww_wide$location == loc, ]
    loc_ww_fit <- ww_fits_wide[ww_fits_wide$location == loc, ]
    loc_ww_obs <- ww_obs[ww_obs$location == loc, ]

    if (nrow(loc_ww_forecast) > 0) {
      n_ww_sites_loc <- min(3, length(unique(loc_ww_forecast$facet_col)))

      p_ww <- create_ww_plot(
        loc_ww_forecast,
        loc_ww_fit,
        loc_ww_obs,
        ylabs$ww,
        n_ww_sites_loc,
        max_ww_sites
      )

      # Combine hospital and wastewater plots
      # Give hospital plots more space relative to wastewater
      return(patchwork::wrap_plots(
        list(p_hosp, p_ww),
        nrow = 1,
        widths = c(0.7 * max_ww_sites, max_ww_sites),
        guides = "collect"
      ))
    }
  }

  # Return just hospital plot if no wastewater data
  return(p_hosp)
}

#' Create multi-location comparison plot with hospital and wastewater fits
#'
#' Creates a comprehensive plot comparing model fits with and without wastewater
#' data across multiple locations (excluding ARIMA models). The plot shows both
#' hospital admission forecasts and wastewater data fits by site for multiple
#' forecast dates. Hospital locations are shown in rows, and wastewater sites
#' are shown as additional columns. Based on get_plot_model_comparison from
#' EDA_plots.R.
#'
#' @param output_path Path to the output folder containing forecast data
#' @param forecast_dates Character vector of forecast dates (e.g.,
#'   c("2024-07-01", "2024-12-09"))
#' @param locations Character vector of location names. If NULL, three random
#'   locations are selected from the first forecast date. Default is NULL.
#' @param forecast_horizon_to_plot Integer indicating number of days of horizon
#'   to plot. Default is 28.
#' @param historical_data_to_plot Integer indicating number of days into the
#'   past to plot. Default is 90.
#' @param scale_selected Character string indicating which scale to plot,
#'   default is "natural"
#' @param save_path Optional path to save the figure. If NULL, figure is not
#'   saved. Default is NULL.
#' @param show_multiple_dates Logical indicating whether to show all forecast
#'   dates or just one. If FALSE, only uses single_date_index to select one
#'   date. Default is TRUE.
#' @param single_date_index Integer index of which forecast date to use when
#'   show_multiple_dates is FALSE. Default is 1 (first date).
#'
#' @return A combined ggplot object with hospital and wastewater plots
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate bind_rows rename select distinct pull
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point facet_grid
#'   facet_wrap ggtitle xlab ylab ggsave theme element_text element_blank
#'   element_rect element_line labs vars geom_vline
#' @importFrom lubridate ymd days
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom patchwork plot_layout wrap_plots plot_spacer plot_annotation
#' @importFrom ggh4x facet_grid2
#' @importFrom grid textGrob gpar
#' @importFrom gridExtra arrangeGrob
#' @export
#' @autoglobal
plot_multilocation_comparison <- function(
    output_path,
    forecast_dates,
    locations = NULL,
    forecast_horizon_to_plot = 28,
    historical_data_to_plot = 90,
    scale_selected = "natural",
    save_path = NULL,
    show_multiple_dates = TRUE,
    single_date_index = 1) {
  # Validate inputs
  if (length(forecast_dates) == 0) {
    stop("forecast_dates must not be empty", call. = FALSE)
  }

  # If show_multiple_dates is FALSE, select specific forecast date by index
  if (!show_multiple_dates) {
    forecast_dates <- forecast_dates[single_date_index]
  }

  # Get locations from first forecast date if not specified
  if (is.null(locations)) {
    first_forecast_path <- file.path(
      output_path,
      "individual_forecasts_all_runs",
      forecast_dates[1]
    )
    available_locations <- list.dirs(
      first_forecast_path,
      full.names = FALSE,
      recursive = FALSE
    )
    locations <- sample(
      available_locations,
      size = min(3, length(available_locations))
    )
  }

  # Load forecast data using helper functions
  hosp_forecasts_list <- load_hospital_forecasts(
    output_path, forecast_dates, locations
  )
  ww_forecasts_list <- load_ww_forecasts(
    output_path, forecast_dates, locations
  )
  ww_later_obs_list <- load_later_ww_obs(
    output_path, forecast_dates, locations
  )

  # Combine and process hospital forecasts
  hosp_forecasts <- bind_rows(hosp_forecasts_list)

  # Check if we have data
  if (nrow(hosp_forecasts) == 0) {
    stop(
      "No hospital forecast data found for the selected locations",
      call. = FALSE
    )
  }

  # Process hospital data using helper function
  hosp_processed <- process_hospital_data(
    hosp_forecasts,
    forecast_horizon_to_plot,
    historical_data_to_plot,
    scale_selected
  )

  forecasts_wide <- hosp_processed$forecasts
  fits_wide <- hosp_processed$fits
  hosp_obs <- hosp_processed$observations
  min_forecast_date <- hosp_processed$min_forecast_date
  min_date_filter <- hosp_processed$min_date_filter
  max_date_filter <- hosp_processed$max_date_filter

  # Process wastewater data if available
  ww_wide <- NULL
  ww_fits_wide <- NULL
  ww_obs <- NULL
  if (length(ww_forecasts_list) > 0) {
    ww_forecasts <- bind_rows(ww_forecasts_list)
    ww_processed <- process_ww_data(
      ww_forecasts,
      ww_later_obs_list,
      forecast_horizon_to_plot,
      min_date_filter,
      max_date_filter,
      min_forecast_date
    )
    ww_wide <- ww_processed$forecasts
    ww_fits_wide <- ww_processed$fits
    ww_obs <- ww_processed$observations
  }

  # Create plots for each location
  if (!is.null(ww_wide)) {
    # Calculate max number of WW sites across all locations
    sites_per_location <- table(
      ww_wide[
        !duplicated(ww_wide[, c("location", "facet_col")]),
        "location"
      ]
    )
    max_ww_sites <- max(sites_per_location)

    # Create plots for each location using helper function
    # nolint start: unnecessary_lambda_linter.
    # Lambda is necessary here because create_location_plot() requires
    # 9 arguments, but we're only iterating over locations (one argument).
    # The other 8 arguments need to be captured from the enclosing scope.
    location_plots <- lapply(locations, function(loc) {
      return(create_location_plot(
        loc, forecasts_wide, fits_wide, hosp_obs,
        ww_wide, ww_fits_wide, ww_obs, locations, max_ww_sites
      ))
    })
    # nolint end

    p_combined <- patchwork::wrap_plots(
      location_plots,
      ncol = 1,
      guides = "collect"
    ) +
      patchwork::plot_annotation(
        title = glue(
          "Model Comparison ({length(forecast_dates)} ",
          "forecast date",
          "{if (length(forecast_dates) != 1) 's'}"
        )
      ) &
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.box.just = "center",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0.5, "cm")
      )
  } else {
    # Just hospital data - facet by location only
    p_combined <- ggplot() +
      forecast_ribbon_layers(forecasts_wide) +
      geom_point(
        data = hosp_obs,
        aes(x = date_parsed, y = observed),
        color = "black"
      ) +
      facet_wrap(~location, ncol = 1, scales = "free_y") +
      model_ww_color_scales() +
      lshtm_theme() +
      xlab("") +
      ylab("7-day rolling sum of hospital admissions") +
      ggtitle(
        glue(
          "Hospital Forecast Comparison ({length(forecast_dates)} forecast dates)" # nolint
        )
      )
  }

  # Save plot if path provided
  if (!is.null(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    date_range <- glue("{min(forecast_dates)}_to_{max(forecast_dates)}")

    # Calculate dimensions based on number of locations and facets
    n_locations <- length(locations)
    # Height: 3 inches per location row + 1.5 for title/legend
    plot_height <- (n_locations * 3) + 1.5

    # Width: If we have wastewater data, make wider to accommodate panels
    if (!is.null(ww_wide)) {
      # Generous width for hospital + wastewater panels
      plot_width <- 16
    } else {
      # Just hospital data - narrower plot is fine
      plot_width <- 10
    }

    ggsave(
      filename = file.path(
        save_path,
        glue("multilocation_comparison_{date_range}.png")
      ),
      plot = p_combined,
      width = plot_width,
      height = plot_height
    )
  }

  return(p_combined)
}
