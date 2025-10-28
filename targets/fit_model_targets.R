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
  tar_target(
    name = model_spec,
    command = get_model_spec(
      generation_interval = generation_interval,
      inf_to_count_delay = inf_to_hosp,
      infection_feedback_pmf = infection_feedback_pmf,
      params = params,
      include_ww = scenarios$include_ww
    ),
    pattern = map(scenarios),
    iteration = "list"
  ),
  tar_target(
    name = fit_opts,
    command = list(
      seed = 123,
      iter_sampling = iter_sampling,
      iter_warmup = iter_warmup
    )
  ),
  # Fit the model to each set of hosp and ww data for each permutation
  tar_target(
    name = hosp_quantiles_wwinference,
    command = fit_wwinference_wrapper(
      ww_data = ww_data_to_fit,
      count_data = hosp_data_preprocessed,
      this_forecast_date = scenarios$forecast_date,
      calibration_time = calibration_period_wwinference,
      forecast_horizon = forecast_horizon,
      model_spec = model_spec,
      fit_opts = fit_opts,
      quantiles_to_save = quantiles_to_save,
      compiled_model = compiled_model,
      hosp_data_eval = hosp_data_eval
    ),
    pattern = map(
      ww_data_to_fit, hosp_data_preprocessed, scenarios,
      model_spec, hosp_data_eval
    )
  )
)
