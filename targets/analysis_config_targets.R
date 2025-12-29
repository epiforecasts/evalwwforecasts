analysis_config_targets <- list(
  tar_target(
    ww_data_post,
    get_ww_as_of_forecast_date(
      forecast_date = scenarios$forecast_date,
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      calibration_period = calibration_period_wwinference,
      path_to_lod_vals = path_to_lod_vals
    ),
    pattern = map(scenarios)
  )
)
