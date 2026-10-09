analysis_EDA_plot_targets <- list(
  tar_target(
    name = scores_to_plot,
    command = mutate(
      filter(scores, model != "arima_baseline"),
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
  )
)
