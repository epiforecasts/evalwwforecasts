run_gam_targets <- list(
  tar_target(
    name = scores_raw,
    command = read_csv(scores_fp)
  ),
  tar_target(
    name = scores,
    command = convert_to_su_object(scores_raw)
  ),
  tar_target(
    name = scores_to_model,
    command = prep_scores_to_model(
      scores_long = scores,
      ww_metadata = ww_metadata
    )
  ),
  # Descriptive plots of relationship between performance and wastewater
  # characteristics
  tar_target(
    name = plot_smooth_scores_vs_ww,
    command = exploratory_plot_ww_vs_scores(
      scores = as.data.frame(scores),
      ww_metadata = ww_metadata,
      plot_type = "continuous",
      fig_file_name = "smooths_score_vs_ww"
    )
  ),
  tar_target(
    name = plot_binned_scores_vs_ww,
    command = exploratory_plot_ww_vs_scores(
      scores = as.data.frame(scores),
      ww_metadata = ww_metadata,
      plot_type = "discrete",
      fig_file_name = "binned_score_vs_ww"
    )
  ),
  # Run the GAM model-------------------------------------------------------
  # with all scores as outcomes and wastewater
  # presence/absence as a covariate.

  # This is the one we want to focus on
  tar_target(
    name = gam_results_long,
    command = fit_gam_long(
      scores_to_model = scores_to_model
    )
  ),
  tar_target(
    name = plot_fixed_effects_location,
    command = get_plot_effect_by_location(gam_results_long)
  ),
  tar_target(
    name = plot_effect_ww,
    command = get_plot_effect_ww(gam_results_long)
  ),
  tar_target(
    name = plot_effects_ww_characteristics,
    command = get_plot_ww_chars(gam_results_long,
      vars =
        c(
          "avg_sampling_freq", "min_latency",
          "pop_coverage", "n_sites"
        )
    )
  ),
  tar_target(
    name = plot_effects_forecast_covars,
    command = get_plot_ww_chars(gam_results_long,
      vars =
        c("horizon", "forecast_date_num", "state_pop")
    )
  ),
  tar_target(
    name = plot_gam_diagnostics,
    command = get_plot_gam_diagnostics(gam_results_long)
  )
)
