get_metadata_targets <- list(
  tar_target(
    name = state_pop_data,
    command = get_state_pop_data()
  ),
  tar_target(
    name = ww_metadata_table,
    command = if (nrow(ww_data_post) > 0) {
      calculate_ww_metadata_table(
        ww_data = ww_data_post,
        state_pop_data = state_pop_data,
        sampling_freq_window = calibration_period_wwinference
      )
    } else {
      tibble()
    },
    pattern = map(ww_data_post)
  ),
  tar_target(
    name = ww_metadata_table_combined,
    command = {
      combined <- bind_rows(ww_metadata_table)
      output_dir <- file.path("metadata")
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }
      write_csv(combined, file.path(output_dir, "ww_metadata_table.csv"))
      return(combined)
    }
  )
)
