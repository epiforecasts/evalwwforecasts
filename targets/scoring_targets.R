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
  tar_target(
    name = score_hosp,
    command = score_samples(
      draws = hosp_draws_w_data,
      forecast_date = scenarios$forecast_date,
      offset = 1
    ),
    pattern = map(hosp_draws_w_data, scenarios)
  )
)
