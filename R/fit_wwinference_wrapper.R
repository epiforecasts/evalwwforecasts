fit_wwinference_wrapper <- function(
    ww_data,
    count_data,
    hosp_data_eval,
    this_forecast_date,
    model_spec,
    fit_opts,
    compiled_model,
    calibration_time = 90,
    forecast_horizon = 28,
    quantiles_to_save = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    fig_file_path = file.path("output", "figs")) {
  loc <- unique(count_data$state)
  include_ww <- model_spec$include_ww
  ww_fit_obj <- wwinference(
    ww_data = ww_data,
    count_data = count_data,
    forecast_date = this_forecast_date,
    calibration_time = calibration_time,
    forecast_horizon = forecast_horizon,
    model_spec = model_spec,
    fit_opts = fit_opts,
    compiled_model = compiled_model
  )

  hosp_draws <- get_draws(ww_fit_obj,
    what = "predicted_counts"
  )$predicted_counts
  # Save plots
  dir.create(fig_file_path)
  full_fp <- file.path(fig_file_path, this_forecast_date, loc)
  dir.create(file.path(fig_file_path, this_forecast_date))
  dir.create(full_fp)
  plot_hosp_draws <- get_plot_forecasted_counts(
    draws = hosp_draws,
    forecast_date = this_forecast_date
  ) + ggtitle(ggtitle(glue("{loc}, wastewater: {include_ww}")))

  ggsave(
    plot = plot_hosp_draws,
    filename = file.path(
      full_fp,
      glue::glue("hosp_draws_ww_{include_ww}.png")
    )
  )
  ww_draws <- if (!is.null(ww_fit_obj$raw_input_data$input_ww_data)) {
    get_draws(ww_fit_obj, what = "predicted_ww")$predicted_ww
  } else {
    NULL
  }

  if (!is.null(ww_draws)) {
    plot_ww_draws <- get_plot_ww_conc(
      draws = ww_draws,
      forecast_date = this_forecast_date
    ) + ggtitle(glue("{loc}, wastewater: {include_ww}")) # nolint
    ggsave(
      plot = plot_ww_draws,
      filename = file.path(
        full_fp,
        "ww_draws.png"
      )
    )
  }

  draws_w_data <- get_model_draws_w_data(
    fit_obj_wwinference = ww_fit_obj,
    model_output = "hosp",
    include_ww = include_ww,
    model = "wwinference",
    forecast_date = this_forecast_date,
    location = loc,
    eval_data = hosp_data_eval
  )

  # hosp_quantiles <- draws_for_scoring(
  #   draws = draws_w_data,
  #   forecast_date = this_forecast_date,
  #   offset = 1,
  #   quantiles = TRUE,
  #   probs = quantiles_to_save
  # ) |> as.data.frame()

  # quantiles <- draws_for_scoring(
  #   draws = test,
  #   forecast_date = "2025-03-22",
  #   offset = 1,
  #   quantiles = TRUE
  # )

  return(hosp_draws)
}
