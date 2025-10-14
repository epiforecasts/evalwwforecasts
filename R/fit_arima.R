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
#' @param forecast_horizon Integer indicating the forecast horizon
#'
#' @returns `forecast_df` Data.frame of forecasts results in wide format
fit_arima <- function(hosp_data_for_fit,
                      hosp_data_for_eval,
                      forecast_date,
                      location_name,
                      location_abbr,
                      data_right_trunc,
                      include_ww,
                      model,
                      forecast_horizon = 28) {
  auto_arima_model <- auto.arima(hosp_data_for_fit$updated_hosp_7d_count,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE
  )

  forecast_result <- forecast(auto_arima_model, h = forecast_horizon)

  forecast_df <- data.frame(
    date = seq(
      from = ymd(forecast_date),
      to = ymd(forecast_date) + days(forecast_horizon - 1), by = "days"
    ),
    point_forecast = as.numeric(forecast_result$mean),
    lower_80 = as.numeric(forecast_result$lower[, "80%"]),
    upper_80 = as.numeric(forecast_result$upper[, "80%"]),
    lower_95 = as.numeric(forecast_result$lower[, "95%"]),
    upper_95 = as.numeric(forecast_result$upper[, "95%"])
  ) |>
    mutate(
      forecast_date = forecast_date,
      location_name = location_name,
      location_abbr = location_abbr,
      data_right_trunc = data_right_trunc,
      include_ww = include_ww,
      model = model
    )
  # Add a join of the evaluation data, later pivot from wide to long for
  # scoring but is fine for now as this will be better for intermediate vis

  return(forecast_df)
}
