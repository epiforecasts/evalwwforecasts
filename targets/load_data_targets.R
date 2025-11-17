load_data_targets <- list(
  tar_target(
    hosp_data_eval,
    get_hosp_for_eval(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date,
      forecast_horizon = forecast_horizon
    ),
    pattern = map(scenarios)
  ),
  tar_target(
    hosp_data,
    get_hosp_for_fit(
      hosp_data_eval = hosp_data_eval,
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date,
      forecast_horizon = forecast_horizon,
      calibration_period = calibration_period_wwinference,
      hosp_data_real_time = scenarios$hosp_data_real_time
    ),
    pattern = map(hosp_data_eval, scenarios)
  ),
  tar_target(
    ww_data_eval,
    get_ww_for_eval(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date,
      path_to_lod_vals = path_to_lod_vals,
    ),
    pattern = map(scenarios)
  ),
  tar_target(
    ww_data,
    get_ww_as_of_forecast_date(
      forecast_date = scenarios$forecast_date,
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      calibration_period = calibration_period_wwinference,
      path_to_lod_vals = path_to_lod_vals
    ),
    pattern = map(ww_data_eval, scenarios)
  )
)
