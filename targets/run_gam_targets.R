run_gam_targets <- list(
  tar_target(
    name = scores_raw,
    command = read_csv(scores_fp)
  ),
  tar_target(
    name = scores,
    command = convert_to_su_object(scores_raw)
  ),
  # Exclude location-forecast dates where any model run failed the
  # convergence checks, for all models. `scores` (all forecasts) is kept for
  # the sensitivity analysis including all forecasts.
  tar_target(
    name = scores_included,
    command = get_scores_included(scores)
  ),
  tar_target(
    name = diagnostics,
    command = read_csv(diagnostics_fp)
  ),
  tar_target(
    name = excluded_forecasts,
    command = check_excluded_forecasts(
      scores = scores,
      excluded_forecasts = get_excluded_forecasts(diagnostics)
    )
  ),
  tar_file(
    name = save_excluded_forecasts,
    command = save_csv(
      excluded_forecasts,
      "excluded_forecasts.csv",
      path = file.path("output", "tables")
    )
  ),
  # Descriptive plots of relationship between performance and wastewater
  # characteristics
  tar_target(
    name = plot_smooth_scores_vs_ww,
    command = exploratory_plot_ww_vs_scores(
      scores = as.data.frame(scores_included),
      ww_metadata = ww_metadata,
      plot_type = "continuous",
      fig_file_name = "smooths_score_vs_ww"
    )
  ),
  tar_target(
    name = plot_binned_scores_vs_ww,
    command = exploratory_plot_ww_vs_scores(
      scores = as.data.frame(scores_included),
      ww_metadata = ww_metadata,
      plot_type = "discrete",
      fig_file_name = "binned_score_vs_ww"
    )
  ),
  # Run the GAMs -----------------------------------------------------------
  # Long-format scores for the 2x2 design: renewal (wwinference) and simple
  # statistical (ww_regression) models, each with and without wastewater,
  # on the same set of forecasts
  tar_target(
    name = scores_long_to_model,
    command = prep_scores_long_to_model(
      scores_long = scores_included,
      ww_metadata = ww_metadata
    )
  ),
  # (a) Separate GAMs for each model type, with the same structure
  tar_target(
    name = gam_results_long,
    command = fit_gam_long(
      scores_long_to_model = scores_long_to_model,
      model_type = "renewal"
    ),
    format = "rds"
  ),
  tar_target(
    name = gam_results_long_statistical,
    command = fit_gam_long(
      scores_long_to_model = scores_long_to_model,
      model_type = "statistical"
    ),
    format = "rds"
  ),
  # (b) One GAM combining all four models
  tar_target(
    name = gam_results_combined,
    command = fit_gam_combined(
      scores_long_to_model = scores_long_to_model
    ),
    format = "rds"
  ),
  tar_target(
    name = combined_ww_effects,
    command = get_combined_ww_effects(gam_results_combined)
  ),
  tar_target(
    name = plot_combined_ww_effects,
    command = get_plot_combined_ww_effects(gam_results_combined),
    format = "rds"
  ),
  tar_target(
    name = plot_gam_diagnostics_combined,
    command = get_plot_gam_diagnostics(gam_results_combined),
    format = "rds"
  ),
  # Relative WIS of including wastewater by epidemic phase, for each model
  # type, from the separate and combined GAMs
  tar_target(
    name = ww_effect_by_phase,
    command = bind_rows(
      lapply(
        list(
          gam_results_long, gam_results_long_statistical,
          gam_results_combined
        ),
        get_ww_effect_by_phase
      )
    )
  ),
  tar_target(
    name = plot_ww_effect_by_phase,
    command = get_plot_ww_effect_by_phase(
      list(
        gam_results_long, gam_results_long_statistical,
        gam_results_combined
      )
    ),
    format = "rds"
  ),
  tar_target(
    name = ww_effect_by_location,
    command = bind_rows(lapply(
      list(gam_results_long, gam_results_long_statistical),
      get_ww_effect_by_location
    ))
  ),
  tar_target(
    name = plot_ww_effect_by_location,
    command = get_plot_ww_effect_by_location(
      list(gam_results_long, gam_results_long_statistical)
    ),
    format = "rds"
  ),
  # Plots for each of the separate GAMs
  tar_target(
    name = gam_by_model_type,
    command = list(
      renewal = gam_results_long,
      statistical = gam_results_long_statistical
    ),
    format = "rds",
    iteration = "list"
  ),
  tar_target(
    name = plot_fixed_effects_location,
    command = get_plot_effect_by_location(gam_by_model_type),
    pattern = map(gam_by_model_type),
    iteration = "list",
    format = "rds"
  ),
  tar_target(
    name = plot_effect_ww,
    command = get_plot_effect_ww(gam_by_model_type),
    pattern = map(gam_by_model_type),
    iteration = "list",
    format = "rds"
  ),
  tar_target(
    name = plot_effects_ww_characteristics,
    command = get_plot_ww_chars(gam_by_model_type,
      vars =
        c(
          "avg_sampling_freq", "min_latency",
          "pop_coverage", "n_sites",
          "avg_data_variability"
        )
    ),
    pattern = map(gam_by_model_type),
    iteration = "list",
    format = "rds"
  ),
  tar_target(
    name = plot_effects_forecast_covars,
    command = get_plot_ww_chars(gam_by_model_type,
      vars =
        c("horizon", "forecast_date_num", "state_pop")
    ),
    pattern = map(gam_by_model_type),
    iteration = "list",
    format = "rds"
  ),
  tar_target(
    name = plot_gam_diagnostics,
    command = get_plot_gam_diagnostics(gam_by_model_type),
    pattern = map(gam_by_model_type),
    iteration = "list",
    format = "rds"
  )
)
