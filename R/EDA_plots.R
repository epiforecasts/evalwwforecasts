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
