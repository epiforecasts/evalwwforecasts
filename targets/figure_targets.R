figure_targets <- list(
  # Common config for figure outputs
  tar_target(
    name = fig_output_path,
    command = file.path("output", "overall_figs_all_runs")
  ),

  # ===========================================================================
  # Fig 1: Visual comparison for a single forecast date
  # Uses plot_multilocation_comparison() with show_multiple_dates = FALSE
  # Already partly in multilocation_plot_targets, but we define a dedicated

  # target here for the paper figure with specific locations
  # ===========================================================================
  tar_target(
    name = fig1_locations,
    command = c("Bremen", "Hamburg", "Berlin")
  ),
  # Pick a single date index where all locations have WW data
  # (index 1 may be too early for some locations)
  tar_target(
    name = fig1_date_index,
    command = {
      # Find first date where all fig1 locations have ww_quantiles
      for (i in seq_along(selected_forecast_dates_multiloc)) {
        has_all <- all(vapply(fig1_locations, function(loc) {
          file.exists(file.path(
            output_path_multiloc, "individual_forecasts_all_runs",
            selected_forecast_dates_multiloc[i], loc, "data",
            "ww_quantiles.csv"
          ))
        }, logical(1)))
        if (has_all) {
          return(i)
        }
      }
      return(1L)
    }
  ),
  tar_target(
    name = fig1,
    command = plot_multilocation_comparison(
      output_path = output_path_multiloc,
      forecast_dates = selected_forecast_dates_multiloc,
      locations = fig1_locations,
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = file.path(fig_output_path, "fig1"),
      show_multiple_dates = FALSE,
      single_date_index = fig1_date_index
    ),
    format = "rds"
  ),

  # ===========================================================================
  # Fig 2: Visual comparison + scores across forecast dates
  # Uses get_combined_forecast_wis_plot()
  # ===========================================================================
  tar_target(
    name = fig2_forecast_dates,
    command = {
      dates <- sort(as.character(unique(scores$forecast_date)))
      # Select dates spread across the time range
      dates[seq(1, length(dates), by = 2)]
    }
  ),
  tar_target(
    name = fig2,
    command = get_combined_forecast_wis_plot(
      output_path = output_path_multiloc,
      forecast_dates = fig2_forecast_dates,
      scores = scores,
      locations = fig1_locations,
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = file.path(fig_output_path, "fig2"),
      n_forecast_dates = 5
    ),
    format = "rds"
  ),

  # ===========================================================================
  # Fig 3: Score comparison overall, by horizon, by location, by forecast date
  # Uses plot_score_comparison()
  # ===========================================================================
  tar_target(
    name = fig3,
    command = plot_score_comparison(
      scores = scores,
      hosp_data = NULL,
      save_path = file.path(fig_output_path, "fig3")
    ),
    format = "rds"
  )
)
