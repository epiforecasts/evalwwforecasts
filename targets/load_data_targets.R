load_data_targets <- list(
  tar_target(
    hosp_data_eval,
    get_hosp_for_eval(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date
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
      calibration_period = 100,
      right_trunc = scenarios$data_right_trunc
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
    get_ww_as_of_forecast_date(
      forecast_date = scenarios$forecast_date,
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      calibration_period = 100
    ),
    pattern = map(ww_data_eval, scenarios)
  )
)
