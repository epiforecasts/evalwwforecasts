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
  )
)
