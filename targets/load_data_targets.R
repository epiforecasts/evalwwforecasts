load_data_targets <- list(
  tar_target(

    # Note we should refactor so we load in all the hosp data once and then
    # filter to the correct location and truncate based on forecast date
    # so we don't hit the api a ton of times unnecessarily
    hosp_data,
    get_hosp_data(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date,
      right_trunc = scenarios$data_right_trunc
    ),
    pattern = map(scenarios)
  ),
  # Again we should refactor for same reason as above, just making one
  # function for now
  tar_target(
    ww_data,
    get_ww_data(
      location_name = scenarios$location_name,
      location_abbr = scenarios$location_abbr,
      forecast_date = scenarios$forecast_date
    ),
    pattern = map(scenarios),
  )
)
