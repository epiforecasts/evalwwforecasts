#' Compare forecasts across time
#'
#' @param forecasts_w_eval_data Data.frame of forecasts with quantiles and
#'   evaluation data
#' @param hosp_data_long Data.frame of observed data before the first forecast
#'   date through the last
#' @param forecast_horizon_to_plot Integer indicating the number of days to
#'   plot after the last forecast date. Default is `28`
#' @param historical_data_to_plot Integer indicating number of days to plot
#'   before the first forecast date. Default is 90.
#'
#'
#' @returns ggplot object
#' @importFrom dplyr filter distinct pull
#' @importFrom glue glue
#' @importFrom lubridate ymd days
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point xlab
#'   ylab ggtitle theme_bw
#' @autoglobal
plot_forecast_comparison <- function(
    forecasts_w_eval_data,
    hosp_data_long,
    forecast_horizon_to_plot = 28,
    historical_data_to_plot = 90) {
  forecasts_i <- filter(
    forecasts_w_eval_data,
    date <= ymd(forecast_date) + days(forecast_horizon_to_plot - 1)
  )
  min_forecast_date <- min(forecasts_i$forecast_date)
  hosp_data <- filter(
    hosp_data_long,
    date <= ymd(max(forecast_date)) + days(forecast_horizon_to_plot - 1),
    date >= ymd(min_forecast_date) - days(historical_data_to_plot)
  )
  this_location <- forecasts_w_eval_data |>
    distinct(state) |>
    pull(state)
  p <- ggplot(forecasts_i) +
    geom_line(aes(
      x = date, y = q_0.5,
      group = forecast_date
    ), color = "blue") +
    geom_point(
      data = hosp_data,
      aes(x = date, y = updated_hosp_7d_count), color = "black"
    ) +
    geom_ribbon(aes(
      x = date, ymin = q_0.25,
      ymax = q_0.75,
      group = forecast_date
    ), alpha = 0.3, fill = "blue") +
    geom_ribbon(aes(
      x = date, ymin = q_0.05,
      ymax = q_0.95,
      group = forecast_date
    ), alpha = 0.3, fill = "blue") +
    theme_bw() +
    xlab("") +
    ylab("7-day rolling sum of hospital admissions") +
    ggtitle(glue("Forecast comparison for {this_location}"))

  return(p)
}

#' Get a plot comparing models for a single location across multiple forecast
#'   dates
#'
#' @param quantiles_to_score Dataframe for a single location across multiple
#'    forecast dates
#' @param hosp_data_long Dataframe of hospital admissions data for single
#'    location
#' @param forecast_horizon_to_plot Integer indicating number of days of horizon
#'   to plot
#' @param historical_data_to_plot Integer indicating number of days into the
#'   past to plot
#' @param scale_selected Character string indicating which scale to plot,
#'   default is `"natural"`
#' @param facet_models Boolean indicating whether to facet the outputs, default
#'   is FALSE.
#' @param fig_fp Character string indicating file path to save figure.
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 facet_wrap
#' @importFrom fs dir_create
#' @returns ggplot object
#' @autoglobal
get_plot_model_comparison <- function(
    quantiles_to_score,
    hosp_data_long,
    forecast_horizon_to_plot = 28,
    historical_data_to_plot = 90,
    scale_selected = "natural",
    facet_models = FALSE,
    fig_fp) {
  forecasts_i <- quantiles_to_score |>
    filter(
      date <= ymd(forecast_date) + days(forecast_horizon_to_plot - 1),
      scale == scale_selected
    ) |>
    mutate(
      model_ww = glue::glue("{model}-{include_ww}"),
      forecast_date_model_ww = glue::glue(
        "{forecast_date}-{model}-{include_ww}"
      )
    ) |>
    pivot_wider(
      names_from = quantile_level,
      values_from = predicted,
      names_prefix = "q_"
    )
  min_forecast_date <- min(forecasts_i$forecast_date)
  max_forecast_date <- max(forecasts_i$forecast_date)
  hosp_data <- filter(
    hosp_data_long,
    date <= ymd(max_forecast_date) + days(forecast_horizon_to_plot - 1),
    date >= ymd(min_forecast_date) - days(historical_data_to_plot)
  )
  this_location <- quantiles_to_score |>
    distinct(location) |>
    pull(location)
  p <- ggplot(forecasts_i) +
    geom_line(aes(
      x = date, y = q_0.5,
      group = forecast_date_model_ww,
      color = model_ww
    )) +
    geom_point(
      data = hosp_data,
      aes(x = date, y = updated_hosp_7d_count), color = "black"
    ) +
    geom_line(
      data = hosp_data,
      aes(x = date, y = updated_hosp_7d_count), color = "black"
    ) +
    geom_ribbon(aes(
      x = date, ymin = q_0.25,
      ymax = q_0.75,
      group = forecast_date_model_ww,
      fill = model_ww
    ), alpha = 0.3) +
    geom_ribbon(aes(
      x = date, ymin = q_0.05,
      ymax = q_0.95,
      group = forecast_date_model_ww,
      fill = model_ww
    ), alpha = 0.3) +
    theme_bw() +
    xlab("") +
    ylab("7-day rolling sum of hospital admissions") +
    ggtitle(glue("Forecast comparison for {this_location}"))
  if (isTRUE(facet_models)) {
    p <- p + facet_wrap(~model_ww, nrow = 3)
  }
  full_fp <- file.path(fig_fp, this_location)
  if (!file.exists(full_fp)) {
    dir_create(full_fp, recurse = TRUE)
  }
  ggsave(
    plot = p,
    filename = file.path(
      full_fp,
      glue::glue("model_comparison_{this_location}.png")
    )
  )

  return(p)
}

#' Get a plot of the draws with both calibration and evaluation data
#'
#' @param draws_w_data Data.frame of draws with data
#' @param full_fp Directory to save
#' @importFrom ggplot2 geom_vline theme element_text
#' @returns ggplot object
#' @autoglobal
get_plot_draws_w_calib_data <- function(draws_w_data,
                                        full_fp) {
  loc <- unique(draws_w_data$location)
  include_ww <- unique(draws_w_data$include_ww)
  forecast_date <- unique(draws_w_data$forecast_date)
  hosp_data_real_time <- unique(draws_w_data$hosp_data_real_time)
  n_draws <- max(draws_w_data$draw, na.rm = TRUE)
  draws <- draws_w_data |> dplyr::filter(
    draw %in% sample.int(n_draws, size = min(100, n_draws))
  )

  p <- ggplot(draws) +
    geom_line(aes(x = date, y = pred_value7dsum, group = draw),
      size = 0.2, alpha = 0.2, color = "darkred"
    ) +
    geom_point(aes(x = date, y = calib_data_7dsum),
      color = "black"
    ) +
    geom_point(aes(x = date, y = updated_hosp_7d_count),
      color = "black", shape = 1
    ) +
    xlab("") +
    theme_bw() +
    theme(plot.title = element_text(size = 10)) +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    ylab("7-day rolling sum of hospital admissions") +
    ggtitle(glue("location: {loc}, include_ww: {include_ww}, forecast_date: {forecast_date}, hosp data real time: {hosp_data_real_time}")) # nolint
  ggsave(
    plot = p,
    filename = file.path(
      full_fp,
      glue::glue("7d_hosp_draws_w_data_ww_{include_ww}_rt_{hosp_data_real_time}.png") # nolint
    )
  )
  return(p)
}

#' Get bar chart of the overall scores
#'
#' @param scores Data.frame of scores from across locations and forecast dates
#'
#' @importFrom ggplot2 geom_bar
#' @importFrom scoringutils summarise_scores
#' @returns ggplot object
#' @autoglobal
get_bar_chart_overall_scores <- function(scores) {
  scores_summarised <- scores |>
    summarise_scores(by = c("model", "include_ww", "hosp_data_real_time")) |>
    mutate(model_ww = glue::glue("{model}-{include_ww}-{hosp_data_real_time}"))

  p <- ggplot(scores_summarised) +
    geom_bar(
      aes(
        x = model_ww,
        y = wis,
        fill = model_ww
      ),
      stat = "identity",
      position = "dodge"
    ) +
    theme_bw() +
    ggtitle("Scores across all locations and forecast dates")

  scores_by_loc <- scores |>
    summarise_scores(by = c(
      "model", "include_ww",
      "hosp_data_real_time", "forecast_date"
    )) |>
    mutate(model_ww = glue::glue("{model}-{include_ww}-{hosp_data_real_time}"))
  p <- ggplot(scores_by_loc) +
    geom_bar(
      aes(
        x = forecast_date,
        y = wis,
        fill = model_ww
      ),
      stat = "identity",
      position = "dodge"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    ggtitle("Scores across all locations by forecast dates")
  return(p)
}

#' Get bar chart of the scores by forecast date
#'
#' @param scores Data.frame of scores from across locations and forecast dates
#'
#' @importFrom ggplot2 geom_bar
#' @importFrom scoringutils summarise_scores
#' @returns ggplot object
#' @autoglobal
get_plot_scores_by_date <- function(scores) {
  scores_by_date <- scores |>
    summarise_scores(by = c(
      "model", "include_ww",
      "hosp_data_real_time", "forecast_date"
    )) |>
    mutate(model_ww = glue::glue("{model}-{include_ww}-{hosp_data_real_time}"))
  p <- ggplot(scores_by_date) +
    geom_bar(
      aes(
        x = forecast_date,
        y = wis,
        fill = model_ww
      ),
      stat = "identity",
      position = "dodge"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    ggtitle("Scores across all locations by forecast dates")
  return(p)
}

#' Get scatterplot of scores by forecast date and location
#'
#' @param scores Data.frame of scores from across locations and forecast dates
#'
#' @importFrom ggplot2 geom_point geom_line
#' @importFrom scoringutils summarise_scores
#' @returns ggplot object
#' @autoglobal
get_scatterplot_scores <- function(scores) {
  scores_by_forecast <- scores |>
    summarise_scores(by = c(
      "model", "include_ww",
      "hosp_data_real_time", "forecast_date",
      "location"
    )) |>
    filter(model == "wwinference") |>
    pivot_wider(
      names_from = include_ww,
      values_from = wis,
      id_cols = c(forecast_date, location, hosp_data_real_time)
    ) |>
    rename(
      ww_plus_hosp = `TRUE`,
      hosp_only = `FALSE`
    )


  p <- ggplot(scores_by_forecast) +
    geom_point(aes(x = hosp_only, y = ww_plus_hosp)) +
    geom_line(aes(x = hosp_only, y = hosp_only), linetype = "dashed")
  return(p)
}

# ============================================================================
# Helper functions for WIS plotting
# ============================================================================

#' Get standard model colour palette
#'
#' Returns a named vector of colours for consistent model styling across plots.
#'
#' @returns Named character vector of hex colours
#' @keywords internal
get_model_colors <- function() {
  return(c(
    "ARIMA baseline" = "#E57373",
    "With wastewater data" = "#64B5F6",
    "Without wastewater data" = "#81C784"
  ))
}

#' Aggregate and label scores for plotting
#'
#' Aggregates scores by model, location, and forecast date, and adds
#' human-readable model labels.
#'
#' @param scores Data.frame of scores
#' @param locations Optional character vector of locations to filter to
#' @param forecast_dates Optional character vector of forecast dates
#' to filter to
#' @returns Data.frame with aggregated scores and model_label column
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom lubridate ymd
#' @keywords internal
#' @autoglobal
aggregate_scores_for_plot <- function(scores,
                                      locations = NULL,
                                      forecast_dates = NULL) {
  scores_filtered <- scores

  if (!is.null(locations)) {
    scores_filtered <- filter(scores_filtered, location %in% locations)
  }

  if (!is.null(forecast_dates)) {
    scores_filtered <- filter(
      scores_filtered,
      forecast_date %in% forecast_dates
    )
  }

  scores_agg <- scores_filtered |>
    group_by(model, include_ww, hosp_data_real_time, forecast_date, location) |>
    summarise(wis = mean(wis, na.rm = TRUE), .groups = "drop") |>
    mutate(
      model_label = case_when(
        model == "arima_baseline" ~ "ARIMA baseline",
        model == "wwinference" & include_ww ~ "With wastewater data",
        model == "wwinference" & !include_ww ~ "Without wastewater data",
        TRUE ~ glue::glue("{model}-{include_ww}")
      ),
      forecast_date = ymd(forecast_date)
    )
  return(scores_agg)
}

#' Create WIS bar chart for a single location
#'
#' Creates a bar chart showing WIS scores by forecast date for different models.
#'
#' @param scores_data Data.frame with columns: forecast_date, wis, model_label
#' @param model_colors Named vector of colours for models
#' @param show_legend Logical, whether to show legend. Default TRUE.
#' @param title Optional title for the plot
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual theme_bw labs theme
#'   element_text
#' @keywords internal
#' @autoglobal
create_wis_bar_chart <- function(scores_data,
                                 model_colors = get_model_colors(),
                                 show_legend = TRUE,
                                 title = NULL) {
  legend_position <- if (show_legend) "bottom" else "none"

  p <- ggplot(scores_data) +
    geom_bar(
      aes(
        x = forecast_date,
        y = wis,
        fill = model_label
      ),
      stat = "identity",
      position = "dodge"
    ) +
    scale_fill_manual(values = model_colors) +
    theme_bw() +
    labs(
      x = "Forecast Date",
      y = "WIS",
      fill = "Model",
      title = title
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = legend_position
    )

  return(p)
}

# ============================================================================
# Main WIS plotting functions
# ============================================================================

#' Get bar chart of WIS by location and forecast date
#'
#' @param scores Data.frame of scores from across locations and forecast dates
#' @param n_locations Integer indicating number of locations to plot. Default is
#'   3. If NULL, all locations are plotted.
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes geom_bar theme labs facet_wrap
#' @importFrom dplyr filter arrange slice_head group_by summarise pull
#' @export
#' @autoglobal
get_bar_chart_scores_by_loc <- function(scores, n_locations = 3) {
  # Aggregate scores using helper

  scores_agg <- aggregate_scores_for_plot(scores)

  # Select locations to plot
  if (!is.null(n_locations)) {
    # Get top n_locations by average WIS
    top_locations <- scores_agg |>
      group_by(location) |>
      summarise(mean_wis = mean(wis, na.rm = TRUE)) |>
      arrange(mean_wis) |>
      slice_head(n = n_locations) |>
      pull(location)

    scores_agg <- filter(scores_agg, location %in% top_locations)
  }

  # Create bar chart using helper, then add faceting
  model_colors <- get_model_colors()

  p <- create_wis_bar_chart(
    scores_agg,
    model_colors = model_colors,
    show_legend = TRUE,
    title = "WIS by Location and Forecast Date"
  ) +
    facet_wrap(~location, scales = "free_y")

  return(p)
}

#' Create combined forecast and WIS plot by location
#'
#' Creates a two-row plot for each location: top row shows forecast time series
#' (hospital admissions with/without wastewater), bottom row shows WIS over time
#' (including ARIMA baseline). Locations are arranged in columns.
#'
#' The function filters to every other forecast date for readability. It only
#' includes WIS for forecast dates that have corresponding forecast data files,
#' ensuring the time axes align correctly between the forecast and WIS plots.
#'
#' @param output_path Path to the output folder containing forecast data
#' @param forecast_dates Character vector of forecast dates
#' @param scores Data.frame of scores from across locations and forecast dates
#' @param locations Character vector of location names. If NULL, three locations
#'   are selected. Default is NULL.
#' @param forecast_horizon_to_plot Integer indicating number of days of horizon
#'   to plot. Default is 28.
#' @param historical_data_to_plot Integer indicating number of days into the
#'   past to plot. Default is 90.
#' @param scale_selected Character string indicating which scale to plot,
#'   default is "natural"
#' @param save_path Optional path to save the figure. If NULL, figure is not
#'   saved. Default is NULL.
#' @param n_forecast_dates Integer indicating number of forecast dates to show
#'   in the WIS bar charts. Dates are selected spread across the time range.
#'   Default is 3.
#'
#' @returns A combined patchwork plot
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point geom_bar
#'   theme_bw theme element_text labs scale_color_manual scale_fill_manual
#'   scale_x_date
#' @importFrom dplyr filter mutate bind_rows group_by summarise arrange
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate ymd
#' @importFrom patchwork wrap_plots plot_layout
#' @export
#' @autoglobal
get_combined_forecast_wis_plot <- function(
    output_path,
    forecast_dates,
    scores,
    locations = NULL,
    forecast_horizon_to_plot = 28,
    historical_data_to_plot = 90,
    scale_selected = "natural",
    save_path = NULL,
    n_forecast_dates = 3) {
  # Get available forecast dates from the directory that have actual data

  forecasts_dir <- file.path(output_path, "individual_forecasts_all_runs")
  available_forecast_dates <- list.dirs(
    forecasts_dir,
    full.names = FALSE,
    recursive = FALSE
  )

  # Filter to dates that have location subdirectories with actual forecast data
  dates_with_data <- sapply(available_forecast_dates, function(d) {
    date_path <- file.path(forecasts_dir, d)
    subdirs <- list.dirs(date_path, full.names = FALSE, recursive = FALSE)
    # Check if there are actual location subdirectories (German state names)
    # by looking for subdirs that don't match date patterns or error files
    return(any(grepl("^[A-Z]", subdirs) & !grepl("Error|^[0-9]{4}", subdirs)))
  })
  available_forecast_dates <- available_forecast_dates[dates_with_data]

  # Filter to dates that exist in both the input and directory
  forecast_dates_available <-
    forecast_dates[forecast_dates %in% available_forecast_dates]

  if (length(forecast_dates_available) == 0) {
    stop("No matching forecast dates found in directory", call. = FALSE)
  }

  # Select n_forecast_dates evenly spaced in calendar time,
  # enforcing a minimum gap between selected dates
  if (length(forecast_dates_available) > n_forecast_dates) {
    available_parsed <- ymd(forecast_dates_available)
    total_span <- as.numeric(
      max(available_parsed) - min(available_parsed)
    )
    min_gap <- floor(total_span / n_forecast_dates) * 0.6

    # Always include first and last, then greedily fill between
    selected_idx <- c(1L, length(available_parsed))
    remaining_n <- n_forecast_dates - 2
    if (remaining_n > 0) {
      target_dates <- seq(
        min(available_parsed), max(available_parsed),
        length.out = n_forecast_dates
      )
      # Drop first and last targets (already selected)
      target_dates <- target_dates[-c(1, length(target_dates))]
      used_dates <- available_parsed[selected_idx]
      for (td in target_dates) {
        # Find closest available date that respects min_gap from all selected
        candidates <- setdiff(seq_along(available_parsed), selected_idx)
        valid <- vapply(candidates, function(ci) {
          return(all(abs(as.numeric(
            available_parsed[ci] - used_dates
          )) >= min_gap))
        }, logical(1))
        if (any(valid)) {
          valid_candidates <- candidates[valid]
          dists <- abs(as.numeric(
            available_parsed[valid_candidates] - td
          ))
          best <- valid_candidates[which.min(dists)]
        } else {
          # Fallback: pick closest regardless
          dists <- abs(as.numeric(available_parsed[candidates] - td))
          best <- candidates[which.min(dists)]
        }
        selected_idx <- c(selected_idx, best)
        used_dates <- c(used_dates, available_parsed[best])
      }
    }
    forecast_dates_filtered <- sort(
      forecast_dates_available[unique(selected_idx)]
    )
  } else {
    forecast_dates_filtered <- forecast_dates_available
  }

  # Determine locations first if not specified
  if (is.null(locations)) {
    # Get available locations from the first forecast date
    first_forecast_path <- file.path(
      output_path,
      "individual_forecasts_all_runs",
      forecast_dates_filtered[1]
    )
    if (!dir.exists(first_forecast_path)) {
      stop("Forecast directory not found", call. = FALSE)
    }
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

  # Load hospital forecasts using helper function
  hosp_forecasts_list <- load_hospital_forecasts(
    output_path, forecast_dates_filtered, locations
  )

  if (length(hosp_forecasts_list) == 0) {
    stop("No hospital forecast data found", call. = FALSE)
  }

  hosp_forecasts <- bind_rows(hosp_forecasts_list)

  # Process hospital data using helper function
  hosp_processed <- process_hospital_data(
    hosp_forecasts,
    forecast_horizon_to_plot,
    historical_data_to_plot,
    scale_selected
  )

  forecasts_wide <- hosp_processed$forecasts
  hosp_obs <- hosp_processed$observations

  # Filter to selected locations and add model labels
  forecasts_wide <- forecasts_wide |>
    filter(location %in% locations) |>
    mutate(
      model_label = case_when(
        model_ww == "wwinference-TRUE" ~ "With wastewater data",
        model_ww == "wwinference-FALSE" ~ "Without wastewater data",
        TRUE ~ model_ww
      )
    )
  hosp_obs <- filter(hosp_obs, location %in% locations)

  # Process scores using helper function
  scores_filtered <- aggregate_scores_for_plot(
    scores,
    locations = locations,
    forecast_dates = forecast_dates_filtered
  )

  # Get standard color palette
  model_colors <- get_model_colors()

  # Compute shared x-axis limits across forecast and WIS plots
  x_min <- min(hosp_obs$date_parsed, forecasts_wide$date_parsed, na.rm = TRUE)
  x_max <- max(hosp_obs$date_parsed, forecasts_wide$date_parsed, na.rm = TRUE)
  shared_xlim <- c(x_min, x_max)

  # Create plots for each location
  plot_list <- list()

  for (loc in locations) {
    # Forecast plot for this location
    loc_forecasts <- filter(forecasts_wide, location == loc)
    loc_obs <- filter(hosp_obs, location == loc)

    p_forecast <- ggplot() +
      geom_line(
        data = loc_forecasts,
        aes(
          x = date_parsed,
          y = q_0.5,
          group = forecast_date_model_ww,
          color = model_label
        )
      ) +
      geom_ribbon(
        data = loc_forecasts,
        aes(
          x = date_parsed,
          ymin = q_0.25,
          ymax = q_0.75,
          group = forecast_date_model_ww,
          fill = model_label
        ),
        alpha = 0.3
      ) +
      geom_point(
        data = loc_obs,
        aes(x = date_parsed, y = observed),
        color = "black"
      ) +
      scale_color_manual(values = model_colors, guide = "none") +
      scale_fill_manual(values = model_colors, guide = "none") +
      scale_x_date(limits = shared_xlim) +
      theme_bw() +
      labs(
        y = "7-day hospital admissions",
        title = loc
      ) +
      theme(
        axis.title.x = element_blank()
      )

    # WIS plot for this location with aligned x-axis
    loc_scores <- filter(scores_filtered, location == loc)
    p_wis <- create_wis_bar_chart(
      loc_scores,
      model_colors = model_colors,
      show_legend = TRUE
    ) +
      scale_x_date(limits = shared_xlim)

    # Combine forecast and WIS plots vertically
    combined_loc <- patchwork::wrap_plots(
      p_forecast,
      p_wis,
      ncol = 1,
      heights = c(2, 1)
    )

    plot_list[[loc]] <- combined_loc
  }

  # Combine all location plots horizontally
  p_combined <- patchwork::wrap_plots(
    plot_list,
    ncol = length(locations),
    guides = "collect"
  ) &
    theme(legend.position = "bottom")

  # Save if path provided
  if (!is.null(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    date_range <- glue::glue("{min(forecast_dates)}_to_{max(forecast_dates)}")
    ggsave(
      filename = file.path(
        save_path,
        glue::glue("combined_forecast_wis_{date_range}.png")
      ),
      plot = p_combined,
      width = 4 * length(locations),
      height = 10
    )
  }

  return(p_combined)
}
