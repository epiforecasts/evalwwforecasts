scoring_targets <- list(
  tar_target(
    name = hosp_quantiles_for_scoring,
    # Keep the run's overall convergence flag (any_flag) so it is carried
    # through to the scores; drop the other diagnostics
    command = hosp_quantiles_wwinference |>
      filter(
        date >= forecast_date
      ) |>
      rename(any_flag = diag_any_flag) |>
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
        # No MCMC for the baseline, so never flagged for convergence
        mutate(flag_missing_ww = FALSE, any_flag = FALSE) |>
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
  # Score the wastewater predictions in each run's ww_quantiles.csv against
  # the later observed data. Depends on the model fits so the files exist.
  tar_target(
    name = scores_ww,
    command = {
      hosp_quantiles_wwinference
      score_ww_quantile_files(
        scenarios = scenarios,
        ind_filepath = ind_filepath,
        metrics = quantile_metrics
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
  )
)
