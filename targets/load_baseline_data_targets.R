load_baseline_data_targets <- list(
  tar_target(
    hosp_data_eval_bl,
    get_hosp_for_eval(
      location_name = scenarios_baseline$location_name,
      location_abbr = scenarios_baseline$location_abbr,
      forecast_date = scenarios_baseline$forecast_date,
      forecast_horizon = forecast_horizon
    ),
    pattern = map(scenarios_baseline)
  ),
  tar_target(
    hosp_data_bl,
    get_hosp_for_fit(
      hosp_data_eval = hosp_data_eval_bl,
      location_name = scenarios_baseline$location_name,
      location_abbr = scenarios_baseline$location_abbr,
      forecast_date = scenarios_baseline$forecast_date,
      forecast_horizon = forecast_horizon,
      calibration_period = NULL,
      hosp_data_real_time = scenarios_baseline$hosp_data_real_time
    ),
    pattern = map(hosp_data_eval_bl, scenarios_baseline)
  ),
  # Wastewater data as of the forecast date, only needed for the wastewater
  # regression model (NULL otherwise). Uses the full history available.
  tar_target(
    ww_data_bl,
    if (scenarios_baseline$model == "ww_regression" &&
      isTRUE(scenarios_baseline$include_ww)) {
      get_ww_as_of_forecast_date(
        forecast_date = scenarios_baseline$forecast_date,
        location_name = scenarios_baseline$location_name,
        location_abbr = scenarios_baseline$location_abbr,
        calibration_period = 5000,
        path_to_lod_vals = path_to_lod_vals
      )
    } else {
      NULL
    },
    pattern = map(scenarios_baseline),
    format = "rds",
    iteration = "list"
  )
)
