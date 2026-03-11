multilocation_plot_targets <- list(
  # Define output path for multilocation plots
  tar_target(
    name = output_path_multiloc,
    command = file.path("output")
  ),
  # Define figure output path
  tar_target(
    name = multiloc_fig_path,
    command = file.path("output", "overall_figs_all_runs", "multilocation")
  ),
  # Select forecast dates to create multilocation plots for
  # (e.g., first, middle, and last forecast date)
  # Only select dates that actually have complete forecast data
  tar_target(
    name = selected_forecast_dates_multiloc,
    command = {
      dates <- unique(scores$forecast_date)
      dates <- sort(dates)

      # Filter to only dates with complete data
      test_loc <- available_locations_multiloc[1]
      date_strs <- as.character(as.Date(dates, origin = "1970-01-01"))
      valid_dates <- date_strs[vapply(date_strs, function(date_str) {
        test_path <- file.path(
          output_path_multiloc,
          "individual_forecasts_all_runs",
          date_str,
          test_loc,
          "data",
          "hosp_quantiles_ww_TRUE.csv"
        )
        return(file.exists(test_path))
      }, logical(1))]

      # Return every other valid date for clarity
      valid_dates[seq(1, length(valid_dates), by = 2)]
    }
  ),
  # Get available locations from scores
  tar_target(
    name = available_locations_multiloc,
    command = unique(scores$location)
  ),
  # Create multilocation comparison plots with all selected dates
  tar_target(
    name = multilocation_comparison_plots,
    command = plot_multilocation_comparison(
      output_path = output_path_multiloc,
      forecast_dates = selected_forecast_dates_multiloc, # Pass all dates
      locations = NULL, # Randomly select 3 locations
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = multiloc_fig_path
    ),
    format = "rds"
  ),
  # Optional: Create plots for specific locations of interest
  tar_target(
    name = locations_of_interest,
    command = c("Bremen", "Hamburg", "Berlin")
  ),
  tar_target(
    name = multilocation_specific_plots,
    command = plot_multilocation_comparison(
      output_path = output_path_multiloc,
      forecast_dates = selected_forecast_dates_multiloc, # Pass all dates
      locations = locations_of_interest,
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = file.path(multiloc_fig_path, "specific_locations")
    ),
    format = "rds"
  ),
  # Create single-date plots (only first forecast date)
  tar_target(
    name = multilocation_single_date_plots,
    command = plot_multilocation_comparison(
      output_path = output_path_multiloc,
      forecast_dates = selected_forecast_dates_multiloc,
      locations = NULL, # Randomly select 3 locations
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = file.path(multiloc_fig_path, "single_date"),
      show_multiple_dates = FALSE
    ),
    format = "rds"
  ),
  tar_target(
    name = multilocation_specific_single_date_plots,
    command = plot_multilocation_comparison(
      output_path = output_path_multiloc,
      forecast_dates = selected_forecast_dates_multiloc,
      locations = locations_of_interest,
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = file.path(
        multiloc_fig_path, "specific_locations", "single_date"
      ),
      show_multiple_dates = FALSE,
      single_date_index = 2 # Use 2024-07-15 (second date with complete data)
    ),
    format = "rds"
  )
)
