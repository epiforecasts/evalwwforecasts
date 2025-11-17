create_permutations_targets <- list(
  tar_target(
    name = locations,
    command = tibble(
      # nolint start
      location_name = "Berlin",
      location_abbr = "BE"
      # location_name = c(
      #   "Nordrhein-Westfalen", "Baden-Württemberg", "Bayern",
      #   "Rheinland-Pfalz", "Thüringen", "Sachsen", "Berlin",
      #   "Sachsen-Anhalt", "Niedersachsen", "Brandenburg",
      #   "Bremen", "Hessen",
      #   "Schleswig-Holstein", "Mecklenburg-Vorpommern",
      #   "Hamburg", "Saarland"
      # ),
      # location_abbr = c(
      #   "NW", "BW", "BY", "RP", "TH", "SN", "BE", "ST", "NI",
      #   "BB", "HB", "HE", "SH", "MV", "HH", "SL"
      # )
    ),
    deployment = "main"
  ),
  tar_file(
    name = save_locations,
    command = save_csv(locations, "locations.csv",
      path = "metadata/meta"
    ),
    deployment = "main"
  ),
  # Will set this from 2024-07-01 (start of git history availability) to
  # 2025-07-07
  tar_target(
    name = forecast_dates,
    command = tibble(forecast_date = c("2024-07-01", "2024-10-21")),
    # seq(from = ymd("2024-07-01"),
    #             to = ymd("2025-06-30"),
    #             by = "week")
    deployment = "main"
  ),
  # nolint end
  tar_target(
    name = ind_filepath,
    command = file.path("output", "individual_forecasts"),
    deployment = "main"
  ),
  tar_file(
    name = save_forecast_dates,
    command = save_csv(forecast_dates, "forecast_dates.csv",
      path = "metadata/meta"
    ),
    deployment = "main"
  ),
  tar_target(
    name = path_to_lod_vals,
    command = file.path(
      "input", "data",
      "loq_data_RKI_clean.csv"
    ),
    deployment = "main"
  ),
  tar_target(
    name = quantiles_to_save,
    command = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975),
    deployment = "main"
  ),
  tar_target(
    name = prediction_intervals,
    command = c(0.5, 0.9, 0.95)
  ),
  tar_target(
    name = quantiles_to_plot,
    command = c(0.025, 0.25, 0.5, 0.75, 0.975),
    deployment = "main"
  ),
  tar_target(
    name = calibration_period_wwinference,
    command = 90,
    deployment = "main"
  ),
  tar_target(
    name = forecast_horizon,
    command = 28,
    deployment = "main"
  ),
  tar_target(
    name = iter_sampling,
    command = 500
  ),
  tar_target(
    name = iter_warmup,
    command = 250,
    deployment = "main"
  ),
  tar_target(
    name = models,
    command = tibble(
      model = "wwinference"
    ),
    deployment = "main"
  ),
  tar_file(
    name = save_models,
    command = save_csv(models, "models.csv",
      path = "metadata/meta"
    ),
    deployment = "main"
  ),
  tar_target(
    name = ww,
    command = tibble(
      include_ww = c(TRUE, FALSE),
    ),
    deployment = "main"
  ),
  tar_file(
    name = save_ww,
    command = save_csv(ww, "ww.csv",
      path = "metadata/meta"
    )
  ),
  tar_target(
    name = hosp_data_real_time,
    command = tibble(
      hosp_data_real_time = TRUE
    ),
    deployment = "main"
  ),
  tar_file(
    name = save_hosp_data_real_time,
    command = save_csv(hosp_data_real_time, "hosp_data_real_time.csv",
      path = "metadata/meta"
    ),
    deployment = "main"
  ),
  # Create the scenarios table for all wwinference models
  tar_group_by(
    name = scenarios,
    command = crossing(
      locations, forecast_dates, ww, models,
      hosp_data_real_time
    ) |>
      mutate(
        scenario_id = row_number(),
        scenario_name = paste(location_abbr, forecast_date, model,
          ifelse(include_ww, "ww", "no_ww"),
          ifelse(hosp_data_real_time, "hosp_data_rt", "hosp_data_final"),
          sep = "_"
        )
      ),
    scenario_name,
    deployment = "main"
  ),
  tar_target(
    name = baseline_models,
    command = tibble(model = "arima_baseline"),
    deployment = "main"
  ),
  # Create the scenarios table for the baseline models models
  tar_group_by(
    name = scenarios_baseline,
    command = crossing(
      locations, forecast_dates, ww, baseline_models,
      hosp_data_real_time
    ) |>
      filter(!(model == "arima_baseline" & include_ww == TRUE)) |>
      mutate(
        scenario_id = row_number(),
        scenario_name = paste(location_abbr, forecast_date, model,
          ifelse(include_ww, "ww", "no_ww"),
          ifelse(hosp_data_real_time, "real_time", "using_final"),
          sep = "_"
        )
      ),
    scenario_name,
    deployment = "main"
  )
)
