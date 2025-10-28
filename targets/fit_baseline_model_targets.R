fit_baseline_model_targets <- list(
  tar_target(
    name = baseline_forecasts,
    command = fit_arima(
      hosp_data_for_fit = hosp_data_bl,
      hosp_data_for_eval = hosp_data_eval_bl,
      forecast_date = scenarios_baseline$forecast_date,
      data_right_trunc = scenarios_baseline$data_right_trunc,
      include_ww = scenarios_baseline$include_ww,
      model = scenarios_baseline$model,
      prediction_intervals = prediction_intervals
    ),
    pattern = map(hosp_data_bl, hosp_data_eval_bl, scenarios_baseline)
  ),
  tar_target(
    name = baseline_quantiles,
    command = format_baseline_forecasts(
      baseline_forecasts,
      quantiles_to_save
    ),
    pattern = map(baseline_forecasts)
  ),
  tar_group_by(
    name = baseline_forecasts_by_loc,
    command = baseline_forecasts,
    by = state
  ),
  tar_target(
    name = full_hosp_time_series,
    command = hosp_data_eval_bl |>
      filter(forecast_date == max(forecast_date))
  ),
  tar_group_by(
    name = full_hosp_time_series_by_loc,
    command = full_hosp_time_series,
    by = state
  ),
  tar_target(
    name = plot_baseline_forecasts,
    command = plot_forecast_comparison(
      forecasts_w_eval_data = baseline_forecasts_by_loc,
      hosp_data_long = full_hosp_time_series_by_loc
    ),
    pattern = map(
      baseline_forecasts_by_loc,
      full_hosp_time_series_by_loc
    ),
    iteration = "list",
    format = "rds"
  )
  # format for scoring the same as the output from the wwinference model
  # wrapper
)
