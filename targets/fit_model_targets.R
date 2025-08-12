fit_model_targets <- list(
  # Here I am just checking that the mapping works as expected -- that only
  # the hospital admissions from a specific location and forecast date are being
  # used (verifying just from looking at the plot)
  tar_target(
    name = plot_hosp,
    command = hosp_data |> ggplot() +
      geom_line(aes(x = date, y = actual_hosp_7d_count)),
    pattern = map(hosp_data, scenarios),
    format = "rds"
  )
)
