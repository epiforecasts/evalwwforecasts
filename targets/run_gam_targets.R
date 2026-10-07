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

  # Run GAM model -----------------------------------------------------------
  tar_target(
    name = gam_results,
    command = fit_gam(
      scores_to_model = scores_to_model
    )
  ),
  tar_target(
    name = plot_partial_effects,
    command = partial_plot(gam_results)
  ),
  tar_target(
    name = plot_scores_fit,
    command = plot_scores_fit_gam(
      gam_results,
      scores_to_model
    )
  ),
  # Relative WIS implied by the GAM -----------------------------------------
  tar_target(
    name = plot_gam_rel_wis_covariates,
    command = plot_rel_wis_by_covariate(gam_results)
  ),
  tar_target(
    name = plot_gam_rel_wis_time,
    command = plot_rel_wis_by_time(gam_results)
  ),
  tar_target(
    name = plot_gam_rel_wis_location,
    command = plot_rel_wis_by_location(gam_results)
  ),
  tar_target(
    name = gam_effect_sizes,
    command = get_gam_effect_sizes(gam_results)
  ),
  tar_target(
    name = plot_gam_effects,
    command = plot_gam_effect_sizes(gam_results)
  ),
  # Alternative specifications targeting the ratio of mean scores -----------
  tar_target(
    name = gam_results_weighted,
    command = fit_gam(
      scores_to_model = scores_to_model,
      weighted = TRUE
    )
  ),
  tar_target(
    name = gam_results_long,
    command = fit_gam_long(
      scores_to_model = scores_to_model
    )
  ),
  tar_target(
    name = gam_effect_sizes_comparison,
    command = bind_rows(
      unweighted = get_gam_effect_sizes(gam_results),
      weighted = get_gam_effect_sizes(gam_results_weighted),
      long = get_gam_effect_sizes(gam_results_long),
      .id = "model"
    )
  ),
  # Run GLM for interpretability of coefficients---------------------------
  tar_target(
    name = glm_results,
    command = fit_glm(
      scores_to_model = scores_to_model
    )
  ),
  tar_target(
    name = glm_coef_table,
    command = get_glm_coef_table(glm_results)
  )
)
