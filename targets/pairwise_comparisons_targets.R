pairwise_comparisons_targets <- list(
  # Pairwise comparison tests using scoringutils
  tar_target(
    name = scores_for_comparison,
    command = scores |>
      mutate(model = glue::glue("{model}-{include_ww}")) |>
      select(-include_ww, -hosp_data_real_time, -scale, -flag_missing_ww)
  ),
  tar_target(
    name = pairwise_comparisons,
    command = scores_for_comparison |>
      get_pairwise_comparisons()
  ),
  tar_target(
    name = pairwise_comparisons_filtered,
    command = pairwise_comparisons |>
      filter(compare_against == "wwinference-FALSE")
  ),
  tar_target(
    name = pairwise_comparisons_loc,
    command = scores_for_comparison |>
      get_pairwise_comparisons(
        by = "location"
      )
  ),
  tar_target(
    name = pairwise_comparisons_loc_filtered,
    command = pairwise_comparisons_loc |>
      filter(compare_against == "wwinference-FALSE")
  ),
  tar_target(
    name = pairwise_comparisons_forecast_dates,
    command = scores_for_comparison |>
      get_pairwise_comparisons(
        by = "forecast_date"
      )
  ),
  tar_target(
    name = pairwise_comparisons_fd_filtered,
    command = pairwise_comparisons_forecast_dates |>
      filter(compare_against == "wwinference-FALSE")
  ),
  tar_target(
    name = plot_pwc,
    command = plot_pairwise_comparisons(pairwise_comparisons,
                                        type = "mean_scores_ratio"
    )
  ),
  tar_target(
    name = plot_pwc_locs,
    command = plot_pairwise_comparisons(pairwise_comparisons_loc,
                                        type = "mean_scores_ratio"
    ) +
      facet_wrap(~location)
  ),
  tar_target(
    name = plot_pwc_fds,
    command = plot_pairwise_comparisons(pairwise_comparisons_forecast_dates,
                                        type = "mean_scores_ratio"
    ) +
      facet_wrap(~forecast_date)
  )
)
