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
#'
#' @returns ggplot object
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
  hosp_data <- filter(
    hosp_data_long,
    date <= ymd(max(forecast_date)) + days(forecast_horizon_to_plot - 1),
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
    p <- p + facet_wrap(~model_ww)
  }
  full_fp <- file.path(fig_fp, this_location)
  if (!file.exists(full_fp)) {
    dir_create(full_fp)
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
#'
#' @returns ggplot object
#' @autoglobal
get_plot_draws_w_calib_data <- function(draws_w_data,
                                        full_fp) {
  loc <- unique(draws_w_data$location)
  include_ww <- unique(draws_w_data$include_ww)
  forecast_date <- unique(draws_w_data$forecast_date)

  draws <- draws_w_data |> dplyr::filter(
    draw %in% sample(1:max(draws_w_data$draw), 100)
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
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    ylab("7-day rolling sum of hospital admissions") +
    ggtitle(glue("location: {loc}, include_ww: {include_ww}, forecast_date: {forecast_date}")) # nolint
  ggsave(
    plot = p,
    filename = file.path(
      full_fp,
      glue::glue("7d_hosp_draws_w_data_ww_{include_ww}.png")
    )
  )
  return(p)
}

#' Get bar chart of the overall scores
#'
#' @param scores Data.frame of scores from across locations and forecast dates
#'
#' @returns ggplot object
#' @autoglobal
get_bar_chart_overall_scores <- function(scores) {
  scores_summarised <- scores |>
    summarise_scores(by = c("model", "include_ww")) |>
    mutate(model_ww = glue::glue("{model}-{include_ww}"))

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
    ggtitle("Scores across all locations/forecast dates")
  return(p)
}
