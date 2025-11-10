#' Fit ARIMA baseline model
#'
#' @param hosp_data_for_fit Dataframe of data for a specific location and
#'    forecast date to fit to.
#' @param hosp_data_for_eval Dataframe of data for a specific location and
#'    forecast date to evaluate against.
#' @inheritParams get_hosp_for_eval
#' @param data_right_trunc Boolean indicating whether data is form initial
#'   reports (right-truncated) or updated.
#' @param include_ww Boolean indicating whether wastewater is included.
#' @param model Character string indicating the name of the model.
#' @param prediction_intervals Numeric between 0 and 1 indicating the symmetric
#'   prediction intervals, default is the 50th and 90th so `c(0.5, 0.9)`.
#' @param forecast_horizon Integer indicating the forecast horizon
#'
#' @importFrom dplyr filter mutate
#' @importFrom forecast auto.arima forecast
#' @importFrom lubridate ymd days
#' @returns `forecast_df` Data.frame of forecasts results in wide format
#' @autoglobal
fit_arima <- function(hosp_data_for_fit,
                      hosp_data_for_eval,
                      forecast_date,
                      data_right_trunc,
                      include_ww,
                      model,
                      prediction_intervals = c(0.5, 0.90), # this will get the lower and upper bounds not these specific quantiles #nolint
                      forecast_horizon = 28) {
  hosp_data_eval_forecast <- hosp_data_for_eval |>
    filter(
      date >= ymd(forecast_date),
      date <= ymd(forecast_date) + days(forecast_horizon)
    )
  hosp_data_real_time <- unique(hosp_data_for_fit$hosp_data_real_time)
  auto_arima_model <- auto.arima(hosp_data_for_fit$updated_hosp_7d_count,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE,
    lambda = "auto"
  )

  forecast_result <- forecast(auto_arima_model,
    h = forecast_horizon + 1,
    level = 100 * prediction_intervals
  )

  interval_labels <- paste0(100 * prediction_intervals, "%")

  forecast_df <- data.frame(
    date = seq(
      from = ymd(forecast_date),
      to = ymd(forecast_date) + days(forecast_horizon), by = "days"
    ),
    q_0.5 = as.numeric(forecast_result$mean),
    # later make this programmatic to the prediction intervals
    q_0.25 = as.numeric(forecast_result$lower[, interval_labels[1]]),
    q_0.75 = as.numeric(forecast_result$upper[, interval_labels[1]]),
    q_0.05 = as.numeric(forecast_result$lower[, interval_labels[2]]),
    q_0.95 = as.numeric(forecast_result$upper[, interval_labels[2]]),
    q_0.025 = as.numeric(forecast_result$lower[, interval_labels[3]]),
    q_0.975 = as.numeric(forecast_result$upper[, interval_labels[3]])
  ) |>
    mutate(
      data_right_trunc = data_right_trunc,
      include_ww = include_ww,
      model = model
    ) |>
    left_join(hosp_data_eval_forecast,
      by = "date"
    ) |>
    mutate(
      forecast_date = ymd(forecast_date),
      hosp_data_real_time = hosp_data_real_time
    )
  return(forecast_df)
}
