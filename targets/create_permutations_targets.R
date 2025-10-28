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
      forecast_date = c(
        "2025-01-18",
        "2025-02-15", "2025-03-22", "2025-04-19"
      )
    )
  ),
  tar_target(
    name = ind_filepath,
    command = file.path("output", "individual_forecasts")
  ),
  tar_file(
    name = save_forecast_dates,
    command = save_csv(forecast_dates, "forecast_dates.csv",
      path = "metadata/meta"
    )
  ),
  tar_target(
    name = quantiles_to_save,
    command = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
  ),
  tar_target(
    name = prediction_intervals,
    command = c(0.5, 0.9, 0.95)
  ),
  tar_target(
    name = quantiles_to_plot,
    command = c(0.025, 0.25, 0.5, 0.75, 0.975)
  ),
  tar_target(
    name = calibration_period_wwinference,
    command = 90
  ),
  tar_target(
    name = forecast_horizon,
    command = 28
  ),
  tar_target(
    name = iter_sampling,
    command = 500
  ),
  tar_target(
    name = iter_warmup,
    command = 250
  ),
  tar_target(
    name = models,
    command = tibble(
      model = "wwinference"
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
      include_ww = FALSE # c(TRUE, FALSE),
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
  # Create the scenarios table for all wwinference models
  tar_group_by(
    name = scenarios,
    command = crossing(
      locations, forecast_dates, ww, models,
      right_trunc
    ) |>
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
  tar_target(
    name = baseline_models,
    command = tibble(model = "arima_baseline")
  ),
  # Create the scenarios table for the baseline models models
  tar_group_by(
    name = scenarios_baseline,
    command = crossing(
      locations, forecast_dates, ww, baseline_models,
      right_trunc
    ) |>
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
  )
)
