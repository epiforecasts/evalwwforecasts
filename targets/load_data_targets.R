load_data_targets <- list(
  tar_target(
    hosp_data_eval,
    get_hosp_for_eval(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date,
      right_trunc = scenarios$data_right_trunc
    ),
    pattern = map(scenarios),
    format = "rds",
    iteration = "list"
  ),
  tar_target(
    hosp_data,
    get_hosp_for_fit(
      hosp_data_eval = hosp_data_eval,
      forecast_date = scenarios$forecast_date,
      calibration_period = 100
    ),
    pattern = map(hosp_data_eval, scenarios)
  ),
  tar_target(
    ww_data_eval,
    get_ww_for_eval(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date
    ),
    pattern = map(scenarios)
  ),
  tar_target(
    ww_data,
    get_ww_for_fit(
      ww_data_eval = ww_data_eval,
      forecast_date = scenarios$forecast_date,
      calibration_period = 100
    ),
    pattern = map(ww_data_eval, scenarios)
  )
)
