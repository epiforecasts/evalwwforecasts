create_permutations_targets <- list(
  tar_target(
    name = locations,
    command = tibble(
      location_name = c("Berlin", "Hamburg"),
      location_abbr = c("BE", "HH")
    )
  ),
  tar_file(
    name = save_locations,
    command = save_csv(locations, "locations.csv",
      path = "metadata/meta"
    )
  ),
  tar_target(
    name = forecast_dates,
    command = tibble(
      forecast_date = c("2025-03-22", "2025-06-27")
    )
  ),
  tar_file(
    name = save_forecast_dates,
    command = save_csv(forecast_dates, "forecast_dates.csv",
      path = "metadata/meta"
    )
  ),
  tar_target(
    name = models,
    command = tibble(
      model = c("wwinference", "arima_baseline")
    )
  ),
  tar_file(
    name = save_models,
    command = save_csv(models, "models.csv",
      path = "metadata/meta"
    )
  ),
  tar_target(
    name = ww,
    command = tibble(
      include_ww = c(TRUE, FALSE),
    )
  ),
  tar_file(
    name = save_ww,
    command = save_csv(ww, "ww.csv",
      path = "metadata/meta"
    )
  ),
  tar_target(
    name = right_trunc,
    command = tibble(
      data_right_trunc = FALSE
    )
  ),
  tar_file(
    name = save_right_trunc,
    command = save_csv(right_trunc, "right_trunc.csv",
      path = "metadata/meta"
    )
  ),
  # Group by all combinations
  tar_group_by(
    name = scenarios,
    command = crossing(
      locations, forecast_dates, ww, models,
      right_trunc
    ) |>
      # ARIMA baseline is only for without wastewater
      filter(!(model == "arima_baseline" & include_ww == TRUE)) |>
      mutate(
        scenario_id = row_number(),
        scenario_name = paste(location_abbr, forecast_date, model,
          ifelse(include_ww, "ww", "no_ww"),
          ifelse(data_right_trunc, "trunc", "no_trunc"),
          sep = "_"
        )
      ),
    scenario_name
  ),
  # Group by just date location with/without wastewater
  tar_group_by(
    name = date_loc_ww_scenarios,
    command = crossing(
      locations, forecast_dates, ww
    ) |>
      mutate(
        scenario_name = paste(location_abbr, forecast_date,
          ifelse(include_ww, "ww", "no_ww"),
          sep = "_"
        )
      ),
    scenario_name
  ),
  # Group by just date and location
  tar_group_by(
    name = date_loc_scenarios,
    command = crossing(
      locations, forecast_dates
    ) |>
      mutate(
        scenario_name = paste(location_abbr, forecast_date, sep = "_")
      ),
    scenario_name
  )
)
