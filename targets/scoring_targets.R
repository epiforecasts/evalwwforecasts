scoring_targets <- list(
  tar_target(
    name = hosp_quantiles_for_scoring,
    command = hosp_quantiles_wwinference |>
      filter(date >= scenarios$forecast_date),
    pattern = map(hosp_quantiles_wwinference, scenarios)
  ),
  # Need to ensure both forecasts have the same columns and format and such
  tar_target(
    name = all_quantiles_for_scoring,
    command = bind_rows(
      hosp_quantiles_for_scoring,
      baseline_quantiles |>
        select(colnames(hosp_quantiles_for_scoring))
    )
  ),
  tar_target(
    name = quantiles_df,
    command = as.data.frame(data.table::as.data.table(
      all_quantiles_for_scoring
    ))
  ),
  tar_group_by(
    name = quantiles_by_loc,
    command = quantiles_df,
    by = location
  ),
  tar_target(
    name = plot_model_comparison,
    command = get_plot_model_comparison(
      quantiles_to_score = quantiles_by_loc,
      hosp_data_long = full_hosp_time_series_by_loc,
      fig_fp = file.path("output", "overall_figs")
    ),
    pattern = map(quantiles_by_loc, full_hosp_time_series_by_loc),
    format = "rds",
    iteration = "list"
  ),
  # Target for scoring quantiles
  tar_target(
    name = score_hosp_quantiles,
    command = generate_scores(
      draws = all_quantiles_for_scoring,
      metrics = quantile_metrics,
      fp_data = file.path("output", "overall_data"),
      save_scores = TRUE
    )
  ),
  tar_target(
    name = bar_chart_overall_scores,
    command = get_bar_chart_overall_scores(score_hosp_quantiles),
    format = "rds",
    iteration = "list"
  )
)
