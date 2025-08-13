load_data_targets <- list(
  tar_target(
    hosp_data,
    get_hosp_data(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date,
      right_trunc = scenarios$data_right_trunc
    ),
    pattern = map(scenarios)
  )
)
