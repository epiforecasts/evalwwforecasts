#' LSHTM custom theme for plots
#'
#' A custom ggplot2 theme function using LSHTM branding colours and styling
#' for consistent plot appearance across the package. The theme uses LSHTM's
#' primary colour (#01454F) for borders, text, and strip backgrounds.
#'
#' @author Ciara Judge (theme design)
#'
#' @return A ggplot2 theme object
#' @importFrom ggplot2 theme element_text element_line element_rect
#' @export
#' @autoglobal
lshtm_theme <- function() {
  lshtm_theme <- theme(
    # add border 1)
    panel.border = element_rect(colour = "#01454F", fill = NA, linewidth = 0.5),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x =
      element_line(colour = "steelblue", linetype = 3, linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "aliceblue"),
    panel.grid.major.y =
      element_line(colour = "steelblue", linetype = 3, linewidth = 0.5),
    panel.grid.minor.y = element_line(colour = "aliceblue"),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#01454F"),
    axis.title = element_text(colour = "#01454F"),
    axis.ticks = element_line(colour = "#01454F"),
    # legend at the bottom 6)
    legend.position = "bottom",
    strip.text.x = element_text(colour = "white"),
    strip.text.y = element_text(colour = "white"),
    strip.background = element_rect(
      color = "#01454F", fill = "#01454F", linewidth = 1.5, linetype = "solid"
    ),
    legend.title = element_text(colour = "#01454F", face = "bold"),
    legend.text = element_text(colour = "#01454F")
  )
  return(lshtm_theme)
}

#' Add human-readable model labels
#'
#' Converts `model` and `include_ww` columns into a single `model_label`
#' column with descriptive names used in figures.
#'
#' @param df Data.frame with columns `model` and `include_ww`
#' @return The input data.frame with an additional `model_label` column
#' @importFrom dplyr mutate case_when
#' @importFrom glue glue
#' @export
#' @autoglobal
add_model_labels <- function(df) {
  result <- df |>
    mutate(
      model_label = case_when(
        model == "arima_baseline" ~ "ARIMA baseline",
        model == "wwinference" & include_ww ~ "With wastewater data",
        model == "wwinference" & !include_ww ~ "Without wastewater data",
        TRUE ~ glue("{model}-{include_ww}")
      )
    )
  return(result)
}

#' Save ARIMA baseline quantiles in the same format as wwinference quantiles
#'
#' Converts wide-format ARIMA baseline forecasts to long-format and saves
#' them alongside the wwinference output so that plotting functions can
#' load them.
#'
#' @param baseline_forecasts Data.frame of baseline forecasts (wide format
#'   with q_* columns)
#' @param output_path Path to the output folder (e.g. "output")
#' @return The input data frame (invisibly), called for side effect of saving
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom readr write_csv
#' @importFrom fs dir_create
#' @importFrom dplyr rename mutate select
#' @export
#' @autoglobal
save_baseline_quantiles <- function(baseline_forecasts, output_path) {
  bl_long <- baseline_forecasts |>
    rename(location = state) |>
    pivot_longer(
      cols = starts_with("q_"),
      names_prefix = "q_",
      names_to = "quantile_level",
      values_to = "predicted"
    ) |>
    mutate(
      quantile_level = as.numeric(quantile_level),
      predicted = pmax(predicted, 0),
      scale = "natural",
      include_ww = FALSE,
      observed = updated_hosp_7d_count,
      flag_missing_ww = FALSE
    ) |>
    select(
      observed, model, include_ww, hosp_data_real_time,
      location, forecast_date, date, scale,
      quantile_level, predicted, flag_missing_ww
    )

  # Save per location and forecast date
  for (fd in unique(as.character(bl_long$forecast_date))) {
    for (loc in unique(bl_long$location)) {
      bl_subset <- bl_long[
        as.character(bl_long$forecast_date) == fd &
          bl_long$location == loc,
      ]
      if (nrow(bl_subset) == 0) next
      full_fp <- file.path(
        output_path, "individual_forecasts_all_runs",
        fd, loc, "data"
      )
      dir_create(full_fp, recurse = TRUE)
      write_csv(
        bl_subset,
        file.path(full_fp, "hosp_quantiles_arima.csv")
      )
    }
  }

  return(invisible(baseline_forecasts))
}

#' Load all hospital quantile forecasts as a single data frame
#'
#' Reads all `hosp_quantiles_ww_TRUE.csv` and `hosp_quantiles_ww_FALSE.csv`
#' files from the output directory and binds them into a single data frame.
#' Used for computing PIT calibration curves.
#'
#' @param output_path Path to the output folder
#' @return Data.frame with columns from the quantile CSV files
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows filter
#' @export
#' @autoglobal
load_all_quantiles <- function(output_path) {
  base_path <- file.path(output_path, "individual_forecasts_all_runs")
  all_files <- list.files(
    base_path,
    pattern = "hosp_quantiles_ww_(TRUE|FALSE)\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  return(bind_rows(lapply(all_files, read_csv, show_col_types = FALSE)))
}

#' Resolve the forecast path for a given date
#'
#' Some forecast dates use a flat structure (date/location/) while others
#' have a double-nested structure (date/date/location/). This helper
#' resolves to the correct path.
#'
#' @param output_path Path to the output folder
#' @param forecast_date Character string forecast date
#' @return Resolved path to the directory containing location folders
#' @keywords internal
resolve_forecast_path <- function(output_path, forecast_date) {
  base_path <- file.path(
    output_path, "individual_forecasts_all_runs", forecast_date
  )
  nested_path <- file.path(base_path, forecast_date)
  if (dir.exists(nested_path)) {
    return(nested_path)
  }
  return(base_path)
}

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
    forecast_path <- resolve_forecast_path(output_path, forecast_date)

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
    forecast_path <- resolve_forecast_path(output_path, forecast_date)
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

    later_path <- resolve_forecast_path(output_path, later_forecast_date)
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
#' @return List with processed hospital forecasts and observations
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

  forecasts_wide <- pivot_wider(
    forecasts_filtered,
    names_from = quantile_level,
    values_from = predicted,
    names_prefix = "q_"
  )

  min_forecast_date <- min(forecasts_wide$forecast_date_parsed, na.rm = TRUE)
  max_forecast_date <- max(forecasts_wide$forecast_date_parsed, na.rm = TRUE)
  min_date_filter <- min_forecast_date - days(historical_data_to_plot)
  max_date_filter <- max_forecast_date + days(forecast_horizon_to_plot - 1)

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
  hosp_obs$facet_col <- "Hospital Admissions"
  hosp_obs$data_type <- "observed"

  return(list(
    forecasts = forecasts_wide,
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
#' @return List with processed wastewater forecasts and observations
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

  ww_filtered <- ww_forecasts[
    ww_forecasts$date_parsed >= ww_forecasts$forecast_date_parsed &
      ww_forecasts$date_parsed <= (
        ww_forecasts$forecast_date_parsed +
          days(forecast_horizon_to_plot - 1)
      ),
  ]

  ww_wide <- pivot_wider(
    ww_filtered,
    names_from = quantile_level,
    values_from = predicted,
    names_prefix = "q_"
  )

  ww_wide$facet_col <- ww_wide$lab_site_name
  ww_wide$forecast_date_site <- paste0(
    ww_wide$forecast_date_chr, "-", ww_wide$lab_site_name
  )
  ww_wide$data_type <- "forecast"

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
    observations = ww_obs
  ))
}

#' Create hospital plot for a single location
#'
#' @param loc_hosp_forecast Hospital forecast data for location
#' @param loc_hosp_obs Hospital observations for location
#' @param loc Location name
#' @param locations All locations (for indexing)
#' @param hosp_ylab Y-axis label
#' @return ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point theme
#'   element_text element_blank labs ggtitle ylab
#' @export
#' @autoglobal
create_hospital_plot <- function(loc_hosp_forecast,
                                 loc_hosp_obs,
                                 loc,
                                 locations,
                                 hosp_ylab) {
  p_hosp <- ggplot() +
    geom_line(
      data = loc_hosp_forecast,
      aes(
        x = date_parsed,
        y = q_0.5,
        group = forecast_date_model_ww,
        color = model_ww
      )
    ) +
    geom_ribbon(
      data = loc_hosp_forecast,
      aes(
        x = date_parsed,
        ymin = q_0.25,
        ymax = q_0.75,
        group = forecast_date_model_ww,
        fill = model_ww
      ),
      alpha = 0.3
    ) +
    geom_ribbon(
      data = loc_hosp_forecast,
      aes(
        x = date_parsed,
        ymin = q_0.05,
        ymax = q_0.95,
        group = forecast_date_model_ww,
        fill = model_ww
      ),
      alpha = 0.3
    ) +
    geom_point(
      data = loc_hosp_obs[loc_hosp_obs$obs_timing == "historical", ],
      aes(x = date_parsed, y = observed),
      color = "black"
    ) +
    geom_point(
      data = loc_hosp_obs[loc_hosp_obs$obs_timing == "future", ],
      aes(x = date_parsed, y = observed),
      color = "gray50"
    ) +
    scale_color_manual(
      values = c(
        "arima_baseline" = "#D55E00",
        "wwinference-TRUE" = "#0072B2",
        "wwinference-FALSE" = "#009E73"
      ),
      labels = c(
        "arima_baseline" = "ARIMA baseline",
        "wwinference-TRUE" = "With wastewater data",
        "wwinference-FALSE" = "Without wastewater data"
      ),
      name = "Model"
    ) +
    scale_fill_manual(
      values = c(
        "arima_baseline" = "#D55E00",
        "wwinference-TRUE" = "#0072B2",
        "wwinference-FALSE" = "#009E73"
      ),
      labels = c(
        "arima_baseline" = "ARIMA baseline",
        "wwinference-TRUE" = "With wastewater data",
        "wwinference-FALSE" = "Without wastewater data"
      ),
      name = "Model"
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
    ww = if (is_middle) "Log genome copies per ml" else ""
  ))
}

#' Create wastewater plot for a single location
#'
#' @param loc_ww_forecast Wastewater forecast data for location
#' @param loc_ww_obs Wastewater observations for location
#' @param ww_ylab Y-axis label
#' @param n_ww_sites_loc Number of WW sites for this location
#' @param max_ww_sites Maximum WW sites across all locations
#' @return ggplot or patchwork object
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point facet_wrap
#'   theme element_text element_blank ggtitle ylab
#' @importFrom patchwork plot_spacer wrap_plots
#' @export
#' @autoglobal
create_ww_plot <- function(loc_ww_forecast,
                           loc_ww_obs,
                           ww_ylab,
                           n_ww_sites_loc,
                           max_ww_sites) {
  p_ww_base <- ggplot() +
    geom_line(
      data = loc_ww_forecast,
      aes(x = date_parsed, y = q_0.5, group = forecast_date_site),
      color = "#01454F"
    ) +
    geom_ribbon(
      data = loc_ww_forecast,
      aes(
        x = date_parsed,
        ymin = q_0.25,
        ymax = q_0.75,
        group = forecast_date_site
      ),
      alpha = 0.3,
      fill = "#01454F"
    ) +
    geom_point(
      data = loc_ww_obs[loc_ww_obs$obs_timing == "historical", ],
      aes(x = date_parsed, y = log_genome_copies_per_ml),
      color = "black",
      size = 0.8
    ) +
    geom_point(
      data = loc_ww_obs[loc_ww_obs$obs_timing == "future", ],
      aes(x = date_parsed, y = log_genome_copies_per_ml),
      color = "gray50",
      size = 0.8
    ) +
    facet_wrap(~facet_col, scales = "free_y", nrow = 1) +
    lshtm_theme() +
    ylab(ww_ylab) +
    ggtitle("") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 0.5),
      legend.position = "none"
    )

  n_spacers_needed <- max_ww_sites - n_ww_sites_loc

  if (n_spacers_needed > 0) {
    ww_elements <- list(p_ww_base)
    for (j in seq_len(n_spacers_needed)) {
      ww_elements[[length(ww_elements) + 1]] <- patchwork::plot_spacer()
    }
    ww_widths <- c(n_ww_sites_loc, rep(1, n_spacers_needed))
    p_ww <- patchwork::wrap_plots(ww_elements, nrow = 1, widths = ww_widths)
  } else {
    p_ww <- p_ww_base
  }

  return(p_ww)
}

#' Create combined plot for a single location
#'
#' @param loc Location name
#' @param forecasts_wide Hospital forecast data
#' @param hosp_obs Hospital observations
#' @param ww_wide Wastewater forecast data (can be NULL)
#' @param ww_obs Wastewater observations (can be NULL)
#' @param locations Vector of all location names
#' @param max_ww_sites Maximum WW sites across all locations
#' @return A combined plot for the location
#' @keywords internal
create_location_plot <- function(loc, forecasts_wide, hosp_obs,
                                 ww_wide, ww_obs, locations, max_ww_sites) {
  # Filter data for this location
  loc_hosp_forecast <- forecasts_wide[forecasts_wide$location == loc, ]
  loc_hosp_obs <- hosp_obs[hosp_obs$location == loc, ]

  # Get y-axis labels
  ylabs <- get_yaxis_labels(loc, locations)

  # Create hospital plot
  p_hosp <- create_hospital_plot(
    loc_hosp_forecast, loc_hosp_obs, loc, locations, ylabs$hosp
  )

  # Create wastewater plot if data available
  if (!is.null(ww_wide)) {
    loc_ww_forecast <- ww_wide[ww_wide$location == loc, ]
    loc_ww_obs <- ww_obs[ww_obs$location == loc, ]

    if (nrow(loc_ww_forecast) > 0) {
      n_ww_sites_loc <- length(unique(loc_ww_forecast$facet_col))

      p_ww <- create_ww_plot(
        loc_ww_forecast,
        loc_ww_obs,
        ylabs$ww,
        n_ww_sites_loc,
        max_ww_sites
      )

      # Combine hospital and wastewater plots
      return(patchwork::wrap_plots(
        list(p_hosp, p_ww),
        nrow = 1,
        widths = c(1, max_ww_sites),
        guides = "keep"
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
  hosp_obs <- hosp_processed$observations
  min_forecast_date <- hosp_processed$min_forecast_date
  min_date_filter <- hosp_processed$min_date_filter
  max_date_filter <- hosp_processed$max_date_filter

  # Process wastewater data if available
  ww_wide <- NULL
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
    # 7 arguments, but we're only iterating over locations (one argument).
    # The other 6 arguments need to be captured from the enclosing scope.
    location_plots <- lapply(locations, function(loc) {
      return(create_location_plot(
        loc, forecasts_wide, hosp_obs,
        ww_wide, ww_obs, locations, max_ww_sites
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
        ),
        caption = "Date"
      ) &
      theme(plot.caption = element_text(hjust = 0.5, size = 11))
  } else {
    # Just hospital data - facet by location only
    p_combined <- ggplot() +
      geom_line(
        data = forecasts_wide,
        aes(
          x = date_parsed,
          y = q_0.5,
          group = forecast_date_model_ww,
          color = model_ww
        )
      ) +
      geom_ribbon(
        data = forecasts_wide,
        aes(
          x = date_parsed,
          ymin = q_0.25,
          ymax = q_0.75,
          group = forecast_date_model_ww,
          fill = model_ww
        ),
        alpha = 0.3
      ) +
      geom_ribbon(
        data = forecasts_wide,
        aes(
          x = date_parsed,
          ymin = q_0.05,
          ymax = q_0.95,
          group = forecast_date_model_ww,
          fill = model_ww
        ),
        alpha = 0.3
      ) +
      geom_point(
        data = hosp_obs,
        aes(x = date_parsed, y = observed),
        color = "black"
      ) +
      facet_wrap(~location, ncol = 1, scales = "free_y") +
      scale_color_manual(
        values = c(
          "arima_baseline" = "#D55E00",
          "wwinference-TRUE" = "#0072B2",
          "wwinference-FALSE" = "#009E73"
        ),
        labels = c(
          "arima_baseline" = "ARIMA baseline",
          "wwinference-TRUE" = "With wastewater data",
          "wwinference-FALSE" = "Without wastewater data"
        ),
        name = "Model"
      ) +
      scale_fill_manual(
        values = c(
          "arima_baseline" = "#D55E00",
          "wwinference-TRUE" = "#0072B2",
          "wwinference-FALSE" = "#009E73"
        ),
        labels = c(
          "arima_baseline" = "ARIMA baseline",
          "wwinference-TRUE" = "With wastewater data",
          "wwinference-FALSE" = "Without wastewater data"
        ),
        name = "Model"
      ) +
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

    # Adjust width based on number of facets (cap at 40 inches)
    n_facet_cols <- 1 + ifelse(!is.null(ww_wide),
      length(unique(ww_wide$facet_col)), 0
    )
    plot_width <- min(8 + (n_facet_cols * 3), 40)

    ggsave(
      filename = file.path(
        save_path,
        glue("multilocation_comparison_{date_range}.png")
      ),
      plot = p_combined,
      width = plot_width,
      height = 8
    )
  }

  return(p_combined)
}

#' Create comprehensive score comparison figure (Fig 3)
#'
#' Creates a multi-panel figure comparing forecast performance across models:
#' A. WIS by model (bar chart with underprediction/overprediction/dispersion)
#' B. Relative WIS by horizon
#' C. PIT calibration curve
#' D. rWIS distribution by model (raincloud plot)
#' E. WIS by location
#' F. WIS by forecast date
#' G. Heatmap of rWIS by forecast date and location
#'
#' @param scores A scoringutils scores object
#' @param quantiles_df Data.frame of quantile forecasts with columns
#'   `quantile_level`, `predicted`, `observed`, `model`, and `include_ww`.
#'   Used for the PIT calibration curve (panel C).
#' @param save_path Optional path to save the figure. If NULL, figure is not
#'   saved.
#' @return A patchwork plot combining all panels
#' @importFrom scoringutils summarise_scores
#' @importFrom dplyr filter mutate group_by summarise arrange left_join
#'   semi_join distinct case_when ungroup select pull
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_tile
#'   geom_jitter geom_abline geom_hline annotate coord_flip coord_equal
#'   facet_wrap labs theme element_text element_blank scale_fill_manual
#'   scale_fill_gradient2 scale_color_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 stat_summary ggsave
#' @importFrom ggdist stat_halfeye
#' @importFrom patchwork wrap_plots plot_annotation plot_layout plot_spacer
#' @importFrom lubridate ymd
#' @importFrom glue glue
#' @export
#' @autoglobal
plot_score_comparison <- function(scores,
                                  quantiles_df,
                                  save_path = NULL) {
  model_colors <- get_model_colors()
  component_colors <- c(
    "underprediction" = "#F0E442",
    "overprediction" = "#CC79A7",
    "dispersion" = "#56B4E9"
  )

  # Add model labels
  scores_labelled <- scores |>
    mutate(
      model_label = case_when(
        model == "arima_baseline" ~ "ARIMA baseline",
        model == "wwinference" & include_ww ~ "With wastewater data",
        model == "wwinference" & !include_ww ~ "Without wastewater data",
        TRUE ~ glue("{model}-{include_ww}")
      )
    )

  # --- Panel A: WIS by model with decomposition ---
  scores_overall <- summarise_scores(scores_labelled, by = "model_label")

  scores_decomp <- scores_overall |>
    pivot_longer(
      cols = c("underprediction", "overprediction", "dispersion"),
      names_to = "component",
      values_to = "value"
    )

  p_a <- ggplot(scores_decomp, aes(
    x = model_label, y = value, fill = component
  )) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    scale_fill_manual(values = component_colors, name = "Component") +
    labs(x = NULL, y = "WIS", tag = "A") +
    lshtm_theme()

  # --- Panel B: Relative WIS by horizon ---
  scores_by_horizon <- scores_labelled |>
    mutate(horizon_days = as.numeric(ymd(date) - ymd(forecast_date))) |>
    summarise_scores(by = c("model_label", "horizon_days"))

  hosp_only_wis <- scores_by_horizon |>
    filter(model_label == "Without wastewater data") |>
    select(horizon_days, wis_ref = wis)

  scores_rwis <- scores_by_horizon |>
    left_join(hosp_only_wis, by = "horizon_days") |>
    mutate(rwis = wis / wis_ref)

  p_b <- ggplot(scores_rwis, aes(
    x = horizon_days, y = rwis, color = model_label
  )) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = model_colors, guide = "none") +
    labs(x = "Horizon (days)", y = "Relative WIS", tag = "B") +
    lshtm_theme()

  # --- Panel C: PIT calibration curve ---
  # For each quantile level and model, compute the proportion of observations
  # that fall below the predicted quantile value
  # Filter quantiles to match the same location-forecast_date combos as scores
  valid_combos <- scores_labelled |>
    select(location, forecast_date) |>
    distinct()

  quantiles_labelled <- quantiles_df |>
    filter(
      date >= forecast_date,
      scale == "natural"
    ) |>
    semi_join(valid_combos, by = c("location", "forecast_date")) |>
    mutate(
      model_label = case_when(
        model == "arima_baseline" ~ "ARIMA baseline",
        model == "wwinference" & include_ww ~ "With wastewater data",
        model == "wwinference" & !include_ww ~ "Without wastewater data",
        TRUE ~ glue("{model}-{include_ww}")
      )
    )

  pit_data <- quantiles_labelled |>
    group_by(model_label, quantile_level) |>
    summarise(
      observed_below = mean(observed <= predicted, na.rm = TRUE),
      .groups = "drop"
    )

  p_c <- ggplot(pit_data, aes(
    x = quantile_level, y = observed_below, color = model_label
  )) +
    annotate("rect",
      xmin = 0.05, xmax = 0.95, ymin = 0.05, ymax = 0.95,
      fill = "#E8F5E9", alpha = 0.4
    ) +
    annotate("rect",
      xmin = 0.25, xmax = 0.75, ymin = 0.25, ymax = 0.75,
      fill = "#C8E6C9", alpha = 0.4
    ) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey40"
    ) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = model_colors, guide = "none") +
    scale_x_continuous(
      labels = function(x) paste0(x * 100, "%"),
      breaks = seq(0, 1, 0.25)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      breaks = seq(0, 1, 0.25)
    ) +
    coord_equal() +
    labs(
      x = "Quantile level", y = "Obs < level",
      tag = "C"
    ) +
    lshtm_theme()

  # --- Panel D: rWIS distribution by model (raincloud plot) ---
  # Compute rWIS for all models relative to "Without wastewater data"
  scores_by_date_loc <- scores_labelled |>
    summarise_scores(by = c("model_label", "forecast_date", "location"))

  hosp_only_ref <- scores_by_date_loc |>
    filter(model_label == "Without wastewater data") |>
    select(forecast_date, location, wis_ref = wis)

  all_rwis <- scores_by_date_loc |>
    left_join(hosp_only_ref, by = c("forecast_date", "location")) |>
    mutate(rwis = wis / wis_ref)

  p_d <- ggplot(all_rwis, aes(
    x = model_label, y = rwis, fill = model_label
  )) +
    ggdist::stat_halfeye(
      adjust = 0.5, width = 0.6, justification = -0.2,
      .width = 0, point_colour = NA
    ) +
    geom_jitter(
      aes(color = model_label),
      width = 0.1, alpha = 0.3, size = 0.8
    ) +
    stat_summary(
      fun = median, geom = "point",
      size = 3, color = "black"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    scale_fill_manual(values = model_colors, guide = "none") +
    scale_color_manual(values = model_colors, guide = "none") +
    scale_y_log10() +
    coord_flip() +
    labs(x = NULL, y = "rWIS", tag = "D") +
    lshtm_theme()

  # --- Panel E: WIS by location ---
  scores_by_loc <- scores_labelled |>
    summarise_scores(by = c("model_label", "location"))

  loc_order <- scores_by_loc |>
    filter(model_label == "Without wastewater data") |>
    arrange(wis) |>
    pull(location)

  scores_by_loc$location <- factor(scores_by_loc$location, levels = loc_order)

  p_e <- ggplot(scores_by_loc, aes(
    x = location, y = wis, fill = model_label
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = model_colors, name = "Model") +
    labs(x = "Location", y = "WIS", tag = "E") +
    lshtm_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )

  # --- Panel F: WIS by forecast date ---
  scores_by_date <- scores_labelled |>
    mutate(forecast_date = ymd(forecast_date)) |>
    summarise_scores(by = c("model_label", "forecast_date"))

  p_f <- ggplot(scores_by_date, aes(
    x = forecast_date, y = wis, fill = model_label
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = model_colors, name = "Model") +
    labs(x = "Forecast date", y = "WIS", tag = "F") +
    lshtm_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )

  # --- Panel G: Heatmap of rWIS by date and location ---
  ww_rwis <- all_rwis |>
    filter(model_label == "With wastewater data") |>
    mutate(forecast_date = ymd(forecast_date))

  p_g <- ggplot(ww_rwis, aes(
    x = forecast_date, y = location, fill = rwis
  )) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 1, name = "rWIS"
    ) +
    labs(x = "Forecast date", y = "Location", tag = "G") +
    lshtm_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7)
    )

  # --- Combine all panels ---
  # Layout: row 1 = A, B, C; row 2 = D, E; row 3 = F (full width);
  # row 4 = G (full width)
  p_combined <- patchwork::wrap_plots(
    p_a, p_b, p_c,
    p_d, p_e,
    p_f,
    p_g,
    design = "AABBCC\nDDDEEE\nFFFFFF\nGGGGGG",
    heights = c(1, 1, 1.5, 1.5),
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = "Forecast score comparison across models"
    ) &
    theme(legend.position = "bottom")

  if (!is.null(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    ggsave(
      filename = file.path(save_path, "fig3_score_comparison.png"),
      plot = p_combined,
      width = 18,
      height = 14
    )
  }

  return(p_combined)
}
