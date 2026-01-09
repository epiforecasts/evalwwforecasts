analysis_EDA_plot_targets <- list(
  tar_target(
    name = plot_scores_by_date,
    command = get_plot_scores_by_date(scores)
  ),
  tar_target(
    name = scatterplot_scores,
    command = get_scatterplot_scores(scores)
  )
)
