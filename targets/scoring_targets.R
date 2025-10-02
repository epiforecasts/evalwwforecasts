scoring_targets <- list(

  # Target to combine model draws with observed data for scoring
  tar_target(
    name = hosp_draws_w_data,
    command = get_model_draws_w_data(
      fit_obj_wwinference = ww_fit_obj,
      model_output = "hosp",
      include_ww = scenarios$include_ww,
      model = scenarios$model,
      forecast_date = scenarios$forecast_date,
      location = scenarios$location_name,
      eval_data = hosp_data_eval
    ),
    pattern = map(ww_fit_obj, hosp_data_eval, scenarios)
  ),

  # Target to generate quantiles and log-transform
  tar_target(
    name = hosp_quantiles_for_scoring,
    command = draws_for_scoring(
      draws = hosp_draws_w_data,
      forecast_date = scenarios$forecast_date,
      offset = 1,
      quantiles = TRUE
    ),
    pattern = map(hosp_draws_w_data, scenarios)
  ),

  # Target to generate samples and log-transform
  tar_target(
    name = hosp_samples_for_scoring,
    command = draws_for_scoring(
      draws = hosp_draws_w_data,
      forecast_date = scenarios$forecast_date,
      offset = 1,
      quantiles = FALSE
    ),
    pattern = map(hosp_draws_w_data, scenarios)
  ),

  # Target for scoring quantiles
  tar_target(
    name = score_hosp_quantiles,
    command = generate_scores(
      draws = hosp_quantiles_for_scoring,
      metrics = quantile_metrics
    ),
    pattern = map(hosp_quantiles_for_scoring, scenarios)
  ),

  # Target for scoring
  tar_target(
    name = score_hosp_samples,
    command = generate_scores(
      draws = hosp_samples_for_scoring,
      metrics = sample_metrics
    ),
    pattern = map(hosp_samples_for_scoring, scenarios)
  )
)
