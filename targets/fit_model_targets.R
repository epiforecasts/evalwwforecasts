fit_model_targets <- list(
  # Pull in parameters from the package
  tar_target(
    name = params,
    command = get_params(
      system.file("extdata", "example_params.toml",
        package = "wwinference"
      )
    )
  ),
  tar_target(
    name = ww_data_preprocessed,
    command = wwinference::preprocess_ww_data(
      ww_data,
      conc_col_name = "log_genome_copies_per_ml",
      lod_col_name = "log_lod"
    ),
    pattern = map(ww_data, scenarios)
  ),
  tar_target(
    name = hosp_data_preprocessed,
    command = wwinference::preprocess_count_data(
      hosp_data,
      count_col_name = "daily_hosp_admits",
      pop_size_col_name = "state_pop"
    ),
    pattern = map(hosp_data, scenarios)
  ),
  tar_target(
    name = ww_data_to_fit,
    command = indicate_ww_exclusions(
      ww_data_preprocessed,
      outlier_col_name = "flag_as_ww_outlier",
      remove_outliers = TRUE
    ),
    pattern = map(ww_data_preprocessed, scenarios)
  ),

  # Model targets (the same for all model runs)
  tar_target(
    name = generation_interval,
    command = wwinference::default_covid_gi
  ),
  tar_target(
    name = inf_to_hosp,
    command = wwinference::default_covid_inf_to_hosp
  ),
  tar_target(
    name = infection_feedback_pmf,
    command = generation_interval
  ),
  # Check to make sure a compiled model can be a target. Look at old
  # code. Otherwise we can do within a wrapper function
  tar_target(
    name = compiled_model_file,
    # This is a cmdstanr object so cant pass as a parquet
    command = {
      model_dir <- "compiled_models"
      dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)

      model <- wwinference::compile_model(target_dir = model_dir)

      # Save the model object to an RDS file
      model_path <- file.path(model_dir, "compiled_model.rds")
      saveRDS(model, model_path)

      return(model_path)
    },
    format = "file"
  ),
  tar_target(
    name = compiled_model,
    command = readRDS(compiled_model_file)
  ),

  # Fit the model to each set of hosp and ww data for each permutation
  tar_target(
    name = ww_fit_obj,
    command = wwinference(
      # if no ww, pass in NULL
      ww_data = ww_data_to_fit,
      count_data = hosp_data_preprocessed,
      forecast_date = scenarios$forecast_date,
      calibration_time = 90,
      forecast_horizon = 28,
      model_spec = get_model_spec(
        generation_interval = generation_interval,
        inf_to_count_delay = inf_to_hosp,
        infection_feedback_pmf = infection_feedback_pmf,
        params = params,
        include_ww = scenarios$include_ww
      ),
      fit_opts = list(
        seed = 123,
        iter_sampling = 500,
        iter_warmup = 250
      ),
      compiled_model = compiled_model
    ),
    format = "rds",
    pattern = map(ww_data_to_fit, hosp_data_preprocessed, scenarios),
    iteration = "list"
  ),
  tar_target(
    name = hosp_draws,
    command = get_draws(ww_fit_obj, what = "predicted_counts")$predicted_counts,
    pattern = map(ww_fit_obj, scenarios)
  ),
  tar_target(
    name = plot_hosp_draws,
    command = get_plot_forecasted_counts(
      draws = hosp_draws,
      forecast_date = scenarios$forecast_date
    ) +
      ggtitle(glue("{scenarios$location_name}, wastewater: {scenarios$include_ww}")), # nolint
    pattern = map(hosp_draws, scenarios),
    format = "rds",
    iteration = "list"
  ),

  # Plotting ww fit
  tar_target(
    name = ww_draws,
    command = if (!is.null(ww_fit_obj$raw_input_data$input_ww_data)) {
      get_draws(ww_fit_obj, what = "predicted_ww")$predicted_ww
    } else {
      NULL
    },
    pattern = map(ww_fit_obj, scenarios),
    iteration = "list"
  ),
  tar_target(
    name = plot_ww_draws,
    command = if (!is.null(ww_draws)) {
      get_plot_ww_conc(
        draws = ww_draws,
        forecast_date = scenarios$forecast_date
      ) +
        ggtitle(glue("{scenarios$location_name}, wastewater: {scenarios$include_ww}")) # nolint
    } else {
      NULL
    },
    pattern = map(ww_draws, scenarios),
    format = "rds",
    iteration = "list"
  ),

  # Here I am just checking that the mapping works as expected -- that only
  # the hospital admissions from a specific location and forecast date are being
  # used (verifying just from looking at the plot)
  tar_target(
    name = plot_hosp,
    command = hosp_data |>
      mutate(date = as.Date(date)) |>
      ggplot() +
      geom_line(aes(x = date, y = daily_hosp_admits)),
    pattern = map(hosp_data, scenarios),
    format = "rds",
    iteration = "list"
  ),

  # Doing the same for wastewater data
  tar_target(
    name = plot_ww,
    command = ggplot(ww_data) +
      geom_line(aes(x = date, y = log_genome_copies_per_ml)),
    pattern = map(ww_data, scenarios),
    format = "rds",
    iteration = "list"
  )
)
