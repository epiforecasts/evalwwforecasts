analysis_EDA_plot_targets <- list(
  tar_target(
    name = scores_to_plot,
    command = mutate(
      filter(scores_included, model != "arima_baseline"),
      ww_var = ifelse(include_ww, "ww", "hosp")
    )
  ),
  tar_target(
    name = plot_scores_by_date,
    command = get_plot_scores_by_date(scores_to_plot)
  ),
  tar_target(
    name = scatterplot_scores,
    command = get_scatterplot_scores(scores_to_plot)
  ),
  tar_target(
    name = bar_chart_overall_scores,
    command = get_bar_chart_overall_scores(scores_to_plot)
  ),
  tar_target(
    name = bar_chart_scores_location,
    command = get_plot_scores_by_loc(scores_to_plot)
  ),
  tar_target(
    name = plot_scores_by_horizon,
    command = get_plot_scores_by_horizon(scores_to_plot)
  ),
  # Comparison of all five models on the same forecasts
  tar_target(
    name = scores_all_models,
    command = get_scores_all_models(scores_included)
  ),
  tar_target(
    name = bar_chart_wis_components,
    command = get_bar_chart_wis_components(scores_all_models)
  ),
  tar_target(
    name = plot_rel_wis_ww_by_model_type,
    command = get_plot_rel_wis_by_type(scores_all_models)
  ),
  tar_target(
    name = bar_chart_wis_by_horizon_all_models,
    command = get_bar_chart_wis_by_horizon(scores_all_models)
  ),
  tar_target(
    name = plot_model_comparison_all_models,
    command = bar_chart_wis_components /
      (plot_rel_wis_ww_by_model_type | bar_chart_wis_by_horizon_all_models)
  )
)
