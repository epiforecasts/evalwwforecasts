scoring_targets <- list(
  tar_target(
    name = hosp_quantiles_for_scoring,
    command = hosp_quantiles_wwinference |>
      filter(
        date >= forecast_date
      ) |>
      select(-starts_with("diag_"))
  ),
  # One row of MCMC diagnostics per forecast date, location and model
  tar_target(
    name = fit_diagnostics,
    command = extract_fit_diagnostics(hosp_quantiles_wwinference)
  ),
  tar_target(
    name = save_fit_diagnostics,
    command = save_csv(
      fit_diagnostics,
      "diagnostics.csv",
      path = file.path("output", "overall_data")
    ),
    format = "file"
  ),
  # Need to ensure both forecasts have the same columns and format and such
  tar_target(
    name = all_quantiles_for_scoring,
    command = bind_rows(
      hosp_quantiles_for_scoring,
      baseline_quantiles |>
        mutate(flag_missing_ww = FALSE) |>
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
      fig_fp = file.path("output", "overall_figs"),
      facet_models = TRUE
    ),
    pattern = map(quantiles_by_loc, full_hosp_time_series_by_loc),
    format = "rds",
    iteration = "list"
  ),
  # Epidemic phase over the forecast period for each location and forecast
  # date, from the 28-day percent change in the final hospital admissions and
  # wastewater data. The percent changes are kept so the phases can be
  # reclassified with a different threshold.
  tar_target(
    name = phase_threshold,
    command = 5
  ),
  tar_target(
    name = epidemic_phase_by_run,
    command = get_epidemic_phase(
      hosp_data_eval = hosp_data_eval,
      ww_data_eval = ww_data_eval,
      forecast_date = scenarios$forecast_date,
      location_name = scenarios$location_name,
      horizon = forecast_horizon,
      threshold = phase_threshold
    ),
    pattern = map(hosp_data_eval, ww_data_eval, scenarios)
  ),
  tar_target(
    name = epidemic_phase,
    command = distinct(epidemic_phase_by_run)
  ),
  tar_target(
    name = save_epidemic_phase,
    command = save_csv(
      epidemic_phase,
      "epidemic_phase.csv",
      path = file.path("output", "overall_data")
    ),
    format = "file"
  ),
  # Target for scoring quantiles
  tar_target(
    name = score_hosp_quantiles,
    command = generate_scores(
      draws = all_quantiles_for_scoring,
      metrics = quantile_metrics,
      fp_data = file.path("output", "overall_data"),
      save_scores = TRUE,
      epidemic_phase = epidemic_phase
    )
  ),
  # Score the wastewater predictions in each run's ww_quantiles.csv against
  # the later observed data. Depends on the model fits so the files exist.
  tar_target(
    name = scores_ww,
    command = {
      hosp_quantiles_wwinference
      score_ww_quantile_files(
        scenarios = scenarios,
        ind_filepath = ind_filepath,
        metrics = quantile_metrics,
        epidemic_phase = epidemic_phase
      )
    }
  ),
  tar_target(
    name = save_scores_ww,
    command = save_csv(
      scores_ww,
      "scores_ww.csv",
      path = file.path("output", "overall_data")
    ),
    format = "file"
  ),
  # Exclude from the analysis any location forecast dates which don't
  # contain ww, convert back to scoring utils object
  tar_target(
    name = scores,
    command = score_hosp_quantiles |>
      group_by(location, forecast_date) |>
      filter(!any(flag_missing_ww)) |>
      ungroup() |>
      convert_to_su_object()
  ),
  tar_target(
    name = bar_chart_overall_scores,
    command = get_bar_chart_overall_scores(scores),
    format = "rds",
    iteration = "list"
  ),
  tar_target(
    name = bar_chart_scores_forecast_date,
    command = get_bar_chart_overall_scores(scores),
    format = "rds",
    iteration = "list"
  )
)
