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
#'   dates or just the first one. If FALSE, only the first forecast date will
#'   be plotted. Default is TRUE.
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
    hosp_data_long,
    forecast_horizon_to_plot = 28,
    historical_data_to_plot = 90,
    scale_selected = "natural",
    save_path = NULL,
    show_multiple_dates = TRUE) {
  # If show_multiple_dates is FALSE, randomly select one forecast date
  if (!show_multiple_dates) {
    # Filter to 2024 dates if available
    dates_2024 <- forecast_dates[grepl("^2024-", forecast_dates)]
    if (length(dates_2024) > 0) {
      forecast_dates <- sample(dates_2024, size = 1)
    } else {
      forecast_dates <- sample(forecast_dates, size = 1)
    }
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

  # Read forecast data for all selected locations and forecast dates
  hosp_forecasts_list <- list()
  ww_forecasts_list <- list()

  for (forecast_date in forecast_dates) {
    forecast_path <- file.path(
      output_path,
      "individual_forecasts_all_runs",
      forecast_date
    )

    for (loc in locations) {
      # Read hospital forecasts with WW
      hosp_ww_path <- file.path(
        forecast_path, loc, "data",
        "hosp_quantiles_ww_TRUE.csv"
      )
      if (file.exists(hosp_ww_path)) {
        temp_data <- read_csv(hosp_ww_path, show_col_types = FALSE)
        temp_data$forecast_date_chr <- forecast_date
        hosp_forecasts_list[[paste0(
          forecast_date, "_", loc, "_ww_TRUE"
        )]] <- temp_data
      }

      # Read hospital forecasts without WW
      hosp_no_ww_path <- file.path(
        forecast_path, loc, "data",
        "hosp_quantiles_ww_FALSE.csv"
      )
      if (file.exists(hosp_no_ww_path)) {
        temp_data <- read_csv(hosp_no_ww_path, show_col_types = FALSE)
        temp_data$forecast_date_chr <- forecast_date
        hosp_forecasts_list[[paste0(
          forecast_date, "_", loc, "_ww_FALSE"
        )]] <- temp_data
      }

      # Read wastewater forecasts for all forecast dates
      ww_path <- file.path(forecast_path, loc, "data", "ww_quantiles.csv")
      if (file.exists(ww_path)) {
        ww_data <- read_csv(ww_path, show_col_types = FALSE)
        ww_data$location <- loc
        ww_data$forecast_date_chr <- forecast_date
        ww_forecasts_list[[paste0(forecast_date, "_", loc)]] <- ww_data
      }
    }
  }

  # Load wastewater observations from 8 weeks after the forecast date
  # to get observations that cover the full forecast horizon
  ww_later_obs_list <- list()

  # Find forecast date approximately 8 weeks (56 days) after current
  all_forecast_dirs <- list.dirs(
    file.path(output_path, "individual_forecasts_all_runs"),
    full.names = FALSE,
    recursive = FALSE
  )
  all_forecast_dates <- sort(all_forecast_dirs[grepl("^\\d{4}-\\d{2}-\\d{2}$", all_forecast_dirs)])

  max_current_forecast <- max(forecast_dates)
  target_date <- ymd(max_current_forecast) + days(56)

  # Find the closest forecast date to 8 weeks later
  all_forecast_dates_parsed <- ymd(all_forecast_dates)
  later_dates <- all_forecast_dates_parsed[all_forecast_dates_parsed >= target_date]

  if (length(later_dates) > 0) {
    # Use the first available date at or after 8 weeks
    later_forecast_date <- as.character(min(later_dates))

    for (loc in locations) {
      ww_path <- file.path(
        output_path,
        "individual_forecasts_all_runs",
        later_forecast_date,
        loc,
        "data",
        "ww_quantiles.csv"
      )
      if (file.exists(ww_path)) {
        ww_later_data <- read_csv(ww_path, show_col_types = FALSE)
        ww_later_data$location <- loc
        ww_later_obs_list[[loc]] <- ww_later_data
      }
    }
  }

  # Combine forecasts
  hosp_forecasts <- bind_rows(hosp_forecasts_list)

  # Check if we have data
  if (nrow(hosp_forecasts) == 0) {
    stop("No hospital forecast data found for the selected locations")
  }

  # Filter to show ONLY forecast period (date >= forecast_date)
  # NOT calibration - only the forecasts extending forward
  # Filter to wwinference model only (exclude arima)
  hosp_forecasts$date_parsed <- ymd(hosp_forecasts$date)
  hosp_forecasts$forecast_date_parsed <- ymd(hosp_forecasts$forecast_date_chr)

  forecasts_filtered <- hosp_forecasts[
    hosp_forecasts$date_parsed >= hosp_forecasts$forecast_date_parsed &
      hosp_forecasts$date_parsed <= (
        hosp_forecasts$forecast_date_parsed +
          days(forecast_horizon_to_plot - 1)
      ) &
      hosp_forecasts$scale == scale_selected &
      hosp_forecasts$model == "wwinference",
  ]

  # Create grouping labels following original function
  forecasts_filtered$model_ww <- ifelse(
    forecasts_filtered$include_ww,
    "wwinference-TRUE",
    "wwinference-FALSE"
  )
  forecasts_filtered$forecast_date_model_ww <- paste0(
    forecasts_filtered$forecast_date_chr, "-",
    forecasts_filtered$model_ww
  )

  # Pivot to wide format
  forecasts_wide <- pivot_wider(
    forecasts_filtered,
    names_from = quantile_level,
    values_from = predicted,
    names_prefix = "q_"
  )

  # Extract observation data from the ORIGINAL hosp_forecasts
  # (before filtering). This ensures we get observed data across
  # the full time period including calibration
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
  # De-duplicate - observed value is same across all
  # quantiles/models for a given date
  hosp_obs <- hosp_obs[
    !duplicated(hosp_obs[, c("date_parsed", "location")]),
  ]

  # Mark observations as historical (before forecast) or future (after forecast)
  # Use the minimum forecast date as the cutoff
  hosp_obs$obs_timing <- ifelse(
    hosp_obs$date_parsed < min_forecast_date,
    "historical",
    "future"
  )

  # Prepare hospital data - add faceting columns
  # facet_col is for the column dimension (Hospital, Site1, Site2, etc.)
  # location is for the row dimension
  forecasts_wide$facet_col <- "Hospital Admissions"
  forecasts_wide$data_type <- "forecast"

  hosp_obs$facet_col <- "Hospital Admissions"
  hosp_obs$data_type <- "observed"

  # Process wastewater data if available and combine with
  # hospital data
  combined_plot_data <- list(
    hospital = forecasts_wide,
    hospital_obs = hosp_obs
  )
  ww_site_list <- list()

  if (length(ww_forecasts_list) > 0) {
    ww_forecasts <- bind_rows(ww_forecasts_list)
    ww_forecasts$date_parsed <- ymd(ww_forecasts$date)
    ww_forecasts$forecast_date_parsed <- ymd(ww_forecasts$forecast_date_chr)

    # Filter wastewater data - show only forecast period for
    # predictions
    ww_filtered <- ww_forecasts[
      ww_forecasts$date_parsed >= ww_forecasts$forecast_date_parsed &
        ww_forecasts$date_parsed <= (
          ww_forecasts$forecast_date_parsed +
            days(forecast_horizon_to_plot - 1)
        ),
    ]

    # Pivot wastewater predictions to wide format
    ww_wide <- pivot_wider(
      ww_filtered,
      names_from = quantile_level,
      values_from = predicted,
      names_prefix = "q_"
    )

    # Add faceting column for wastewater sites
    # Use lab_site_name as the column identifier
    ww_wide$facet_col <- ww_wide$lab_site_name
    ww_wide$forecast_date_site <- paste0(
      ww_wide$forecast_date_chr, "-", ww_wide$lab_site_name
    )
    ww_wide$data_type <- "forecast"

    # Extract observed wastewater data - use forecast file observations
    # for historical data, then supplement with next forecast date's
    # observations to fill in the forecast horizon
    ww_obs_all <- ww_forecasts[
      ww_forecasts$date_parsed >= min_date_filter &
        ww_forecasts$date_parsed <= max_date_filter &
        !is.na(ww_forecasts$log_genome_copies_per_ml),
      c(
        "date_parsed", "location", "site", "lab", "lab_site_name",
        "log_genome_copies_per_ml"
      )
    ]
    # Mark these as historical (available at forecast time)
    ww_obs_all$obs_timing <- "historical"

    # Supplement with observations from the next forecast date
    if (length(ww_later_obs_list) > 0) {
      ww_later_combined <- bind_rows(ww_later_obs_list)
      ww_later_combined$date_parsed <- ymd(ww_later_combined$date)

      # Extract observations from next forecast, filtering to forecast
      # horizon of current forecasts
      min_forecast <- min(forecasts_wide$forecast_date_parsed)
      ww_later_obs <- ww_later_combined[
        ww_later_combined$date_parsed >= min_forecast &
          ww_later_combined$date_parsed <= max_date_filter &
          !is.na(ww_later_combined$log_genome_copies_per_ml),
        c(
          "date_parsed", "location", "site",
          "log_genome_copies_per_ml"
        )
      ]

      # Map later observations to the lab_site_name from current forecasts
      # by matching on site and location
      site_mapping <- unique(ww_obs_all[, c("site", "location", "lab_site_name")])
      ww_later_obs <- merge(
        ww_later_obs,
        site_mapping,
        by = c("site", "location"),
        all.x = TRUE
      )

      # Only keep later obs that match existing sites
      ww_later_obs <- ww_later_obs[!is.na(ww_later_obs$lab_site_name), ]

      # Mark these as future (not available at forecast time)
      ww_later_obs$obs_timing <- "future"

      # Combine, keeping forecast file obs when both exist
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

    combined_plot_data$ww <- ww_wide
    combined_plot_data$ww_obs <- ww_obs
  }

  # Create combined plot using ggh4x or custom faceting to avoid
  # redundant facets. We need to use a trick: create separate plots
  # per location and combine them OR use facet_grid with dropped
  # unused facet combinations

  if (!is.null(combined_plot_data$ww)) {
    ww_wide <- combined_plot_data$ww
    ww_obs <- combined_plot_data$ww_obs

    # Calculate max number of WW sites across all locations
    sites_per_location <- table(
      ww_wide[
        !duplicated(ww_wide[, c("location", "facet_col")]),
        "location"
      ]
    )
    max_ww_sites <- max(sites_per_location)

    # Create separate plots for each location to avoid redundant
    # empty facets. Then stack them vertically using patchwork with
    # fixed widths
    location_plots <- list()

    for (loc in locations) {
      # Filter data for this location
      loc_hosp_forecast <- forecasts_wide[forecasts_wide$location == loc, ]
      loc_hosp_obs <- hosp_obs[hosp_obs$location == loc, ]
      loc_ww_forecast <- ww_wide[ww_wide$location == loc, ]
      loc_ww_obs <- ww_obs[ww_obs$location == loc, ]

      # Count how many WW sites this location has
      n_ww_sites_loc <- length(unique(loc_ww_forecast$facet_col))

      # Create separate hospital plot (no faceting needed - just one panel)
      # Add y-axis label only to the middle location (for vertical centering)
      middle_loc_index <- ceiling(length(locations) / 2)
      hosp_ylab <- if (which(locations == loc) == middle_loc_index) {
        "7-day rolling sum of\nhospital admissions"
      } else {
        ""
      }

      p_hosp <- ggplot() +
        # Hospital forecasts (median)
        geom_line(
          data = loc_hosp_forecast,
          aes(
            x = date_parsed,
            y = q_0.5,
            group = forecast_date_model_ww,
            color = model_ww
          )
        ) +
        # Hospital forecast ribbons
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
        # Hospital observations - historical (before forecast date)
        geom_point(
          data = loc_hosp_obs[loc_hosp_obs$obs_timing == "historical", ],
          aes(x = date_parsed, y = observed),
          color = "black"
        ) +
        # Hospital observations - future (during/after forecast date)
        geom_point(
          data = loc_hosp_obs[loc_hosp_obs$obs_timing == "future", ],
          aes(x = date_parsed, y = observed),
          color = "gray50"
        ) +
        lshtm_theme() +
        labs(color = "Model", fill = "Model") +
        ylab(hosp_ylab) +
        ggtitle(loc) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_text(hjust = 0.5)
        )

      # Create wastewater plot if data available
      if (nrow(loc_ww_forecast) > 0) {
        # Add y-axis label only to the middle location (for vertical centering)
        ww_ylab <- if (which(locations == loc) == middle_loc_index) {
          "Log genome copies per ml"
        } else {
          ""
        }

        p_ww_base <- ggplot() +
          # Wastewater forecasts (median)
          geom_line(
            data = loc_ww_forecast,
            aes(x = date_parsed, y = q_0.5, group = forecast_date_site),
            color = "#01454F"
          ) +
          # Wastewater forecast ribbons
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
          # Wastewater observations - historical (available at forecast time)
          geom_point(
            data = loc_ww_obs[loc_ww_obs$obs_timing == "historical", ],
            aes(x = date_parsed, y = log_genome_copies_per_ml),
            color = "black",
            size = 0.8
          ) +
          # Wastewater observations - future (not available at forecast time)
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

        # Add spacers to wastewater plot to match max_ww_sites
        n_spacers_needed <- max_ww_sites - n_ww_sites_loc

        if (n_spacers_needed > 0) {
          ww_elements <- list(p_ww_base)
          for (j in 1:n_spacers_needed) {
            ww_elements[[length(ww_elements) + 1]] <- patchwork::plot_spacer()
          }
          # Wastewater plot gets n_ww_sites_loc width, each spacer gets 1
          ww_widths <- c(n_ww_sites_loc, rep(1, n_spacers_needed))
          p_ww <- patchwork::wrap_plots(ww_elements, nrow = 1, widths = ww_widths)
        } else {
          p_ww <- p_ww_base
        }

        # Combine hospital and wastewater plots
        # Hospital gets 1 unit, wastewater gets max_ww_sites units
        row_elements <- list(p_hosp, p_ww)
        widths <- c(1, max_ww_sites)
        p_loc <- patchwork::wrap_plots(
          row_elements,
          nrow = 1,
          widths = widths,
          guides = "keep"
        )
      } else {
        p_loc <- p_hosp
      }

      location_plots[[loc]] <- list(
        plot = p_loc,
        n_panels = 1 + n_ww_sites_loc
      )
    }

    # Stack all location plots vertically
    # Extract just the plot objects
    plots_only <- lapply(location_plots, function(x) x$plot)

    p_combined <- patchwork::wrap_plots(
      plots_only,
      ncol = 1,
      guides = "collect"
    ) +
      patchwork::plot_annotation(
        title = glue(
          "Model Comparison ({length(forecast_dates)} forecast dates)"
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
      lshtm_theme() +
      xlab("") +
      ylab("7-day rolling sum of hospital admissions") +
      ggtitle(
        glue(
          "Hospital Forecast Comparison ({length(forecast_dates)} forecast dates)" # nolint
        )
      ) +
      labs(color = "Model", fill = "Model")
  }

  # Save plot if path provided
  if (!is.null(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    date_range <- glue("{min(forecast_dates)}_to_{max(forecast_dates)}")

    # Adjust width based on number of facets (cap at 40 inches)
    n_facet_cols <- 1 + ifelse(!is.null(combined_plot_data$ww),
      length(unique(combined_plot_data$ww$facet_col)), 0
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
