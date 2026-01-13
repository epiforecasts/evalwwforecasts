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
        +      test_loc <- available_locations_multiloc[1]
        dates <- unique(scores$forecast_date)
        dates <- sort(dates)
        
        # Filter to only dates with complete data
             valid_dates <- c()
              test_loc <- available_locations_multiloc[1]
              for (test_date in dates) {
                  date_str <- as.character(as.Date(test_date, origin = "1970-01-01"))
                test_path <- file.path(
                     output_path_multiloc,
                      "individual_forecasts_all_runs",
                      date_str,
                      test_loc,
                      "data",
                      "hosp_quantiles_ww_TRUE.csv"
                    )
                 if (file.exists(test_path)) {
                      valid_dates <- c(valid_dates, date_str)
                    }
                }
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
                  file.exists(test_path)
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
  # Read hospital observation data from individual forecast files
  # We'll read from one forecast date to get the full time series
  tar_target(
    name = hosp_data_all_locations,
    command = {
      # Use the most recent forecast date to get full time series
      # Find a forecast date that actually has data by checking one location
      all_dates <- sort(unique(scores$forecast_date), decreasing = TRUE)
      latest_date <- NULL
      for (test_date in all_dates) {
        date_str <- as.character(as.Date(test_date, origin = "1970-01-01"))
        test_loc <- available_locations_multiloc[1]
        test_path <- file.path(
          output_path_multiloc,
          "individual_forecasts_all_runs",
          date_str,
          test_loc,
          "data",
          "hosp_quantiles_ww_TRUE.csv"
        )
        if (file.exists(test_path)) {
          latest_date <- date_str
          break
        }
      }

      # Read observed data from all locations for this date
      hosp_list <- list()
      for (loc in available_locations_multiloc) {
        file_path <- file.path(
          output_path_multiloc,
          "individual_forecasts_all_runs",
          latest_date,
          loc,
          "data",
          "hosp_quantiles_ww_TRUE.csv"
        )
        if (file.exists(file_path)) {
          temp_data <- read_csv(file_path, show_col_types = FALSE)
          # Select columns and filter - use dplyr with all_of to avoid NSE issues
          temp_data <- dplyr::select(temp_data, dplyr::all_of(c("date", "location", "observed")))
          temp_data <- dplyr::distinct(temp_data)
          temp_data <- dplyr::filter(temp_data, !is.na(observed))
          hosp_list[[loc]] <- temp_data
        }
      }
      result <- dplyr::bind_rows(hosp_list)
      result$date_parsed <- lubridate::ymd(result$date)
      result <- result[order(result$location, result$date_parsed), ]
      # Rename date_parsed back to date for consistency with function expectation
      result$date <- result$date_parsed
      result$date_parsed <- NULL
      result
    }
  ),
  # Create multilocation comparison plots with all selected dates
  tar_target(
    name = multilocation_comparison_plots,
    command = plot_multilocation_comparison(
      output_path = output_path_multiloc,
      forecast_dates = selected_forecast_dates_multiloc, # Pass all dates
      locations = NULL, # Randomly select 3 locations
      hosp_data_long = hosp_data_all_locations,
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
      hosp_data_long = hosp_data_all_locations,
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
      hosp_data_long = hosp_data_all_locations,
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
      hosp_data_long = hosp_data_all_locations,
      forecast_horizon_to_plot = 28,
      historical_data_to_plot = 90,
      scale_selected = "natural",
      save_path = file.path(
        multiloc_fig_path, "specific_locations", "single_date"
      ),
      show_multiple_dates = FALSE
    ),
    format = "rds"
  )
)
