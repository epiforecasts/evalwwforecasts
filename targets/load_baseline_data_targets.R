load_baseline_data_targets <- list(
  tar_target(
    hosp_data_eval_bl,
    get_hosp_for_eval(
      location_name = scenarios_baseline$location_name,
      location_abbr = scenarios_baseline$location_abbr,
      forecast_date = scenarios_baseline$forecast_date
    ),
    pattern = map(scenarios_baseline)
  ),
  tar_target(
    hosp_data_bl,
    get_hosp_for_fit(
      hosp_data_eval = hosp_data_eval_bl,
      forecast_date = scenarios_baseline$forecast_date,
      calibration_period = 100,
      right_trunc = scenarios_baseline$data_right_trunc
    ),
    pattern = map(hosp_data_eval_bl, scenarios_baseline)
  )
)
