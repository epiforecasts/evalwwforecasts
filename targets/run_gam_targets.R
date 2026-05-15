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
    name = plots_ww_vs_scores,
    command = exploratory_plot_ww_vs_scores(
      scores = scores,
      ww_metadata = ww_metadata
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
  # Run GLM for interpretability of coefficients---------------------------
  tar_target(
    name = glm_results,
    command = fit_glm(
      scores_to_model = scores_to_model
    )
  )
  # Use the scores to model dataframe to make scatter plots comparing different
  # variables
  # tar_target(
  #   name = scatter_plot_rWIS_vs_horizon,
  #   command = get_scatter_plot_wis_vs_horizon(scores_to_model =
  #                                               scores_to_model)
  # )
)
