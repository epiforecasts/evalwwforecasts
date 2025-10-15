#' Compare forecasts across time
#'
#' @param forecasts_w_eval_data Data.frame of forecasts qith quantiles and
#'   evaluation data
#' @param location Character string indicating the location to be plotted
#' @param forecast_horizon_to_plot Integer indicating the number of days to
#'   plot.
#'
#' @returns ggplot object
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom lubridate ymd days
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point xlab
#'   ylab ggtitle theme_bw
plot_forecast_comparison <- function(
    forecasts_w_eval_data,
    location,
    forecast_horizon_to_plot = 28) {
  forecasts_i <- forecasts_w_eval_data |>
    filter(
      state == location,
      date <= ymd(forecast_date) + days(forecast_horizon_to_plot - 1)
    )

  p <- ggplot(forecasts_i) +
    geom_line(aes(
      x = date, y = q_0.5,
      group = forecast_date
    ), color = "blue") +
    geom_point(aes(x = date, y = updated_hosp_7d_count), color = "black") +
    geom_ribbon(aes(
      x = date, ymin = q_0.25,
      ymax = q_0.75,
      group = forecast_date
    ), alpha = 0.3, fill = "blue") +
    geom_ribbon(aes(
      x = date, ymin = q_0.95,
      ymax = q_0.05,
      group = forecast_date
    ), alpha = 0.3, fill = "blue") +
    theme_bw() +
    xlab("") +
    ylab("7-day rolling sum of hospital admissions") +
    ggtitle(glue("Forecast comparison for {location}"))

  return(p)
}
