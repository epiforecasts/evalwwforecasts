scoring_targets <- list(
  tar_target(
    name = hosp_quantiles_for_scoring,
    command = hosp_quantiles_wwinference |>
      filter(date >= scenarios$forecast_date),
    pattern = map(hosp_quantiles_wwinference, scenarios)
  ),
  # Need to ensure both forecasts have the same columns and format and such
  # tar_target(
  #   name = all_quantiles_for_scoring,
  #   command = bind_rows(hosp_quantiles_for_scoring,
  #                       baseline_forecasts) |>
  #                as_forecast_quantile()
  # ),

  # Target for scoring quantiles
  tar_target(
    name = score_hosp_quantiles,
    command = generate_scores(
      draws = hosp_quantiles_for_scoring,
      metrics = quantile_metrics
    )
  )
)
