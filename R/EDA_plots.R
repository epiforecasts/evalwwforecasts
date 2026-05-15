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

get_scatterplot_wis_vs_horizon <- function(scores_to_model) {
  scores_to_model <- scores_to_model |>
    mutate(rWIS = wis_ww / wis_hosp)

  ggplot(scores_to_model, aes(x = rWIS, y = factor(horizon), fill = factor(horizon))) +
    geom_density_ridges(alpha = 0.7, scale = 0.9) +
    geom_vline(aes(xintercept = 1), linetype = "dashed") +
    scale_x_continuous(trans = "log10", limits = c(1 / 6.5, 6.5)) +
    scale_fill_viridis_d(guide = "none") +
    labs(x = "rWIS", y = "Horizon (days)") +
    theme_bw()

  # Distributions of ww metadata aross Germany
  ggplot(ww_metadata) +
    geom_histogram(aes(x = n_sites))

  ggplot(ww_metadata) +
    geom_histogram(aes(x = min_latency))

  ggplot(ww_metadata) +
    geom_histogram(aes(x = avg_latency))

  ggplot(ww_metadata) +
    geom_histogram(aes(max_sampling_freq))

  ggplot(ww_metadata) +
    geom_histogram(aes(avg_sampling_freq))

  ggplot(ww_metadata) +
    geom_histogram(aes(pop_coverage))

  ggplot(ww_metadata) +
    geom_histogram(aes(prop_below_LOD))
}

#' Explortory plots of rWIS vs wastewater characteristics
#'
#' @param scores dataframe of scores by location, forecast date, model, and
#'   target date
#' @param ww_metadata Dataframe of wastewater metadata
#' @param plot_type Whether you want wastewater characteristics to be
#'   "continuous" or discrete, default is continuous which plots a smooth
#' @param fig_file_name Name of figure to save, default is NULL
#' @param fig_file_dir FP to save figure
#' @importFrom patchwork plot_annotation plot_layout
#' @returns patchwork object
#' @autoglobal
exploratory_plot_ww_vs_scores <- function(scores,
                                          ww_metadata,
                                          plot_type = "continuous",
                                          fig_file_name = NULL,
                                          fig_file_dir = file.path("output", "figs")) { # nolint
  scores_summarised <- scores |>
    summarise_scores(by = c(
      "location", "forecast_date",
      "model", "include_ww"
    )) |>
    mutate(model = glue::glue("{model}_{include_ww}")) |>
    pivot_wider(
      id_cols = c(
        "location", "forecast_date"
      ),
      names_from = "model",
      values_from = "wis",
      names_prefix = "wis_"
    ) |>
    rename(
      wis_ww    = `wis_wwinference_TRUE`,
      wis_hosp  = `wis_wwinference_FALSE`,
      wis_arima = `wis_arima_baseline_FALSE`
    ) |>
    left_join(ww_metadata, by = c(
      "location" = "location_name",
      "forecast_date"
    )) |>
    mutate(rwis = wis_ww / wis_hosp) |>
    as.data.frame()

  if (plot_type == "continuous") {
    p1 <- ggplot(scores_summarised) +
      geom_smooth(aes(x = min_latency, y = rwis), color = "steelblue") +
      geom_point(aes(x = min_latency, y = rwis), alpha = 0.2) +
      geom_hline(aes(yintercept = 1), linetype = "dashed") +
      ggtitle("Minimum latency") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw() +
      xlab("Minimum latency (days) across sites") +
      ylab("rWIS (vs hospital admissions only)")

    p2 <- ggplot(scores_summarised) +
      geom_smooth(aes(x = avg_sampling_freq, y = rwis), color = "purple") +
      geom_point(aes(x = avg_sampling_freq, y = rwis), alpha = 0.2) +
      geom_hline(aes(yintercept = 1), linetype = "dashed") +
      ggtitle("Average sampling frequency") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw() +
      xlab("Average sampling frequncy across sites") +
      ylab("rWIS (vs hospital admissions only)")

    p3 <- ggplot(scores_summarised) +
      geom_smooth(aes(x = n_sites, y = rwis), color = "blue") +
      geom_point(aes(x = n_sites, y = rwis), alpha = 0.2) +
      geom_hline(aes(yintercept = 1), linetype = "dashed") +
      ggtitle("Number of sites") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw() +
      xlab("Number of sites") +
      ylab("rWIS (vs hospital admissions only)")

    p4 <- ggplot(scores_summarised) +
      geom_smooth(aes(x = pop_coverage, y = rwis), color = "orange") +
      geom_point(aes(x = pop_coverage, y = rwis), alpha = 0.2) +
      geom_hline(aes(yintercept = 1), linetype = "dashed") +
      ggtitle("Wastewater population coverage") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw() +
      xlab("Wastewater population coverage") +
      ylab("rWIS (vs hospital admissions only)")

    p5 <- ggplot(scores_summarised) +
      geom_smooth(aes(x = avg_latency, y = rwis), color = "red") +
      geom_point(aes(x = avg_latency, y = rwis), alpha = 0.2) +
      geom_hline(aes(yintercept = 1), linetype = "dashed") +
      ggtitle("Average latency ") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw() +
      xlab("Average latency across sites (days)") +
      ylab("rWIS (vs hospital admissions only)")

    p6 <- ggplot(scores_summarised) +
      geom_smooth(aes(x = avg_data_variability, y = rwis), color = "darkgreen") +
      geom_point(aes(x = avg_data_variability, y = rwis), alpha = 0.2) +
      geom_hline(aes(yintercept = 1), linetype = "dashed") +
      ggtitle("Average data variability ") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw() +
      xlab("Average data variability") +
      ylab("rWIS (vs hospital admissions only)")

    fig_layout <- "
  ABC
  DEF"

    fig <- p1 + p2 + p3 + p4 + p5 + p6 +
      plot_layout(
        design = fig_layout
      ) +
      plot_annotation(
        tag_levels = "A",
        tag_sep = ""
      )
  }


  # For each variable, make reasonable bins across the variable and then
  # make density plots for each bin
  if (plot_type == "discrete") {
    p1 <- scores_summarised |>
      mutate(latency_bin = cut(min_latency,
        breaks = c(0, 7, 14, 21, Inf),
        labels = c("<1 week", "1-2 weeks", "2-3 weeks", "3 weeks+")
      )) |>
      filter(!is.na(latency_bin)) |>
      group_by(latency_bin) |>
      mutate(mean_rwis = mean(rwis)) |> # quartile bins
      ggplot(aes(x = latency_bin, y = rwis)) +
      geom_violin(fill = "steelblue", alpha = 0.5) +
      geom_jitter(width = 0.1, alpha = 0.2) +
      geom_point(aes(x = latency_bin, y = mean_rwis), size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw()

    p2 <- scores_summarised |>
      mutate(freq_bin = cut(avg_sampling_freq,
        breaks = c(0, 1 / 14, 1 / 7, 2 / 7, Inf),
        labels = c(
          "<1 per 2 weeks",
          "between\n 1 per week and\n 1 per 2 weeks",
          "between\n 2 per week and\n1 per week",
          "more than\n 2 per week"
        )
      )) |>
      filter(!is.na(freq_bin)) |>
      group_by(freq_bin) |>
      mutate(mean_rwis = mean(rwis)) |> # quartile bins
      ggplot(aes(x = freq_bin, y = rwis)) +
      geom_violin(fill = "purple", alpha = 0.5) +
      geom_jitter(width = 0.1, alpha = 0.2) +
      geom_point(aes(x = freq_bin, y = mean_rwis), size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw()

    p3 <- scores_summarised |>
      mutate(n_sites_bin = cut(n_sites,
        breaks = c(0, 3, 8, 15, 22, Inf),
        label = c(
          "<3 ",
          "4-8",
          "9-15",
          "16-22",
          "23+"
        )
      )) |>
      filter(!is.na(n_sites_bin)) |>
      group_by(n_sites_bin) |>
      mutate(mean_rwis = mean(rwis)) |> # quartile bins
      ggplot(aes(x = n_sites_bin, y = rwis)) +
      geom_violin(fill = "blue", alpha = 0.5) +
      geom_jitter(width = 0.1, alpha = 0.2) +
      geom_point(aes(x = n_sites_bin, y = mean_rwis), size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw()

    p4 <- scores_summarised |>
      mutate(pop_cov_bin = cut(pop_coverage,
        breaks = c(0, 20, 40, 75, Inf),
        labels = c("<20%", "20-40%", "40-75%", "75+%")
      )) |>
      filter(!is.na(pop_cov_bin)) |>
      group_by(pop_cov_bin) |>
      mutate(mean_rwis = mean(rwis)) |> # quartile bins
      ggplot(aes(x = pop_cov_bin, y = rwis)) +
      geom_violin(fill = "orange", alpha = 0.5) +
      geom_jitter(width = 0.1, alpha = 0.2) +
      geom_point(aes(x = pop_cov_bin, y = mean_rwis), size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_y_continuous(trans = "log10", limits = c(1 / 6, 6)) +
      theme_bw()

    fig_layout <- "
  AB
  CD"

    fig <- p1 + p2 + p3 + p4 +
      plot_layout(
        design = fig_layout
      ) +
      plot_annotation(
        tag_levels = "A",
        tag_sep = ""
      )
  }


  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = fig,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.tiff")
      ),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 20,
      height = 12
    )
    ggsave(
      plot = fig,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 20,
      height = 12,
      dpi = 600
    )
  }

  return(fig)
}
