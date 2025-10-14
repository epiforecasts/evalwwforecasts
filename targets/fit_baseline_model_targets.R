fit_baseline_model_targets <- list(
  tar_target(
    name = baseline_forecasts,
    command = fit_arima(
      hosp_data_for_fit = hosp_data_bl,
      hosp_data_for_eval = hosp_data_eval_bl,
      forecast_date = scenarios_baseline$forecast_date,
      location_name = scenarios_baseline$location_name,
      location_abbr = scenarios_baseline$location_abbr,
      data_right_trunc = scenarios_baseline$data_right_trunc,
      include_ww = scenarios_baseline$include_ww,
      model = scenarios_baseline$model,
      quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
    ),
    pattern = map(hosp_data_bl, scenarios_baseline),
  ),
  # format for scoring the same as the wwinference ones are formatted


)
