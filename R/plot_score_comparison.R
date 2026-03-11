#' Load all hospital quantile forecasts as a single data frame
#'
#' Reads all `hosp_quantiles_ww_TRUE.csv` and `hosp_quantiles_ww_FALSE.csv`
#' files from the output directory and binds them into a single data frame.
#' Used for computing PIT calibration curves.
#'
#' @param output_path Path to the output folder
#' @return Data.frame with columns from the quantile CSV files
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows filter
#' @export
#' @autoglobal
load_all_quantiles <- function(output_path) {
  base_path <- file.path(output_path, "individual_forecasts_all_runs")
  all_files <- list.files(
    base_path,
    pattern = "hosp_quantiles_(ww_(TRUE|FALSE)|arima)\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  return(bind_rows(lapply(all_files, read_csv, show_col_types = FALSE)))
}

#' Create comprehensive score comparison figure (Fig 3)
#'
#' Creates a multi-panel figure comparing forecast performance across models:
#' A. WIS by model (bar chart with underprediction/overprediction/dispersion)
#' B. Relative WIS by horizon
#' C. PIT calibration curve
#' D. rWIS distribution by model (raincloud plot)
#' E. WIS by location
#' F. WIS by forecast date
#' G. Heatmap of rWIS by forecast date and location
#'
#' @param scores A scoringutils scores object
#' @param quantiles_df Data.frame of quantile forecasts with columns
#'   `quantile_level`, `predicted`, `observed`, `model`, and `include_ww`.
#'   Used for the PIT calibration curve (panel C).
#' @param save_path Optional path to save the figure. If NULL, figure is not
#'   saved.
#' @return A patchwork plot combining all panels
#' @importFrom scoringutils summarise_scores
#' @importFrom dplyr filter mutate group_by summarise arrange left_join
#'   semi_join distinct case_when ungroup select pull
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_tile
#'   geom_jitter geom_abline geom_hline annotate coord_flip coord_equal
#'   facet_wrap labs theme element_text element_blank scale_fill_manual
#'   scale_fill_gradient2 scale_color_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 stat_summary ggsave
#' @importFrom ggdist stat_halfeye
#' @importFrom patchwork wrap_plots plot_annotation plot_layout plot_spacer
#' @importFrom lubridate ymd
#' @importFrom glue glue
#' @export
#' @autoglobal
plot_score_comparison <- function(scores,
                                  quantiles_df,
                                  save_path = NULL) {
  model_colors <- get_model_colors()
  component_colors <- c(
    underprediction = "#F0E442",
    overprediction = "#CC79A7",
    dispersion = "#56B4E9"
  )

  # Add model labels
  scores_labelled <- add_model_labels(scores)

  # --- Panel A: WIS by model with decomposition ---
  scores_overall <- summarise_scores(scores_labelled, by = "model_label")

  scores_decomp <- scores_overall |>
    pivot_longer(
      cols = c("underprediction", "overprediction", "dispersion"),
      names_to = "component",
      values_to = "value"
    )

  p_a <- ggplot(scores_decomp, aes(
    x = model_label, y = value, fill = component
  )) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    scale_fill_manual(values = component_colors, name = "Component") +
    labs(x = NULL, y = "WIS", tag = "A") +
    lshtm_theme()

  # --- Panel B: Relative WIS by horizon ---
  scores_by_horizon <- scores_labelled |>
    mutate(horizon_days = as.numeric(ymd(date) - ymd(forecast_date))) |>
    summarise_scores(by = c("model_label", "horizon_days"))

  hosp_only_wis <- scores_by_horizon |>
    filter(model_label == "Without wastewater data") |>
    select(horizon_days, wis_ref = wis)

  scores_rwis <- scores_by_horizon |>
    left_join(hosp_only_wis, by = "horizon_days") |>
    mutate(rwis = wis / wis_ref)

  p_b <- ggplot(scores_rwis, aes(
    x = horizon_days, y = rwis, color = model_label
  )) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = model_colors, guide = "none") +
    labs(x = "Horizon (days)", y = "Relative WIS", tag = "B") +
    lshtm_theme()

  # --- Panel C: PIT calibration curve ---
  # For each quantile level and model, compute the proportion of observations
  # that fall below the predicted quantile value
  # Filter quantiles to match the same location-forecast_date combos as scores
  valid_combos <- scores_labelled |>
    select(location, forecast_date) |>
    distinct()

  quantiles_labelled <- quantiles_df |>
    filter(
      date >= forecast_date,
      scale == "natural"
    ) |>
    semi_join(valid_combos, by = c("location", "forecast_date")) |>
    add_model_labels()

  pit_data <- quantiles_labelled |>
    group_by(model_label, quantile_level) |>
    summarise(
      observed_below = mean(observed <= predicted, na.rm = TRUE),
      .groups = "drop"
    )

  p_c <- ggplot(pit_data, aes(
    x = quantile_level, y = observed_below, color = model_label
  )) +
    annotate("rect",
      xmin = 0.05, xmax = 0.95, ymin = 0.05, ymax = 0.95,
      fill = "#E8F5E9", alpha = 0.4
    ) +
    annotate("rect",
      xmin = 0.25, xmax = 0.75, ymin = 0.25, ymax = 0.75,
      fill = "#C8E6C9", alpha = 0.4
    ) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey40"
    ) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = model_colors, guide = "none") +
    scale_x_continuous(
      labels = function(x) paste0(x * 100, "%"),
      breaks = seq(0, 1, 0.25)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      breaks = seq(0, 1, 0.25)
    ) +
    coord_equal() +
    labs(
      x = "Quantile level", y = "Obs < level",
      tag = "C"
    ) +
    lshtm_theme()

  # --- Panel D: rWIS distribution by model (raincloud plot) ---
  # Compute rWIS for all models relative to "Without wastewater data"
  scores_by_date_loc <- scores_labelled |>
    summarise_scores(by = c("model_label", "forecast_date", "location"))

  hosp_only_ref <- scores_by_date_loc |>
    filter(model_label == "Without wastewater data") |>
    select(forecast_date, location, wis_ref = wis)

  all_rwis <- scores_by_date_loc |>
    left_join(hosp_only_ref, by = c("forecast_date", "location")) |>
    mutate(rwis = wis / wis_ref)

  p_d <- ggplot(all_rwis, aes(
    x = model_label, y = rwis, fill = model_label
  )) +
    ggdist::stat_halfeye(
      adjust = 0.5, width = 0.6, justification = -0.2,
      .width = 0, point_colour = NA
    ) +
    geom_jitter(
      aes(color = model_label),
      width = 0.1, alpha = 0.3, size = 0.8
    ) +
    stat_summary(
      fun = median, geom = "point",
      size = 3, color = "black"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    scale_fill_manual(values = model_colors, guide = "none") +
    scale_color_manual(values = model_colors, guide = "none") +
    scale_y_log10() +
    coord_flip() +
    labs(x = NULL, y = "rWIS", tag = "D") +
    lshtm_theme()

  # --- Panel E: WIS by location ---
  scores_by_loc <- scores_labelled |>
    summarise_scores(by = c("model_label", "location"))

  loc_order <- scores_by_loc |>
    filter(model_label == "Without wastewater data") |>
    arrange(wis) |>
    pull(location)

  scores_by_loc$location <- factor(scores_by_loc$location, levels = loc_order)

  p_e <- ggplot(scores_by_loc, aes(
    x = location, y = wis, fill = model_label
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = model_colors, name = "Model") +
    labs(x = "Location", y = "WIS", tag = "E") +
    lshtm_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )

  # --- Panel F: WIS by forecast date ---
  scores_by_date <- scores_labelled |>
    mutate(forecast_date = ymd(forecast_date)) |>
    summarise_scores(by = c("model_label", "forecast_date"))

  p_f <- ggplot(scores_by_date, aes(
    x = forecast_date, y = wis, fill = model_label
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = model_colors, name = "Model") +
    labs(x = "Forecast date", y = "WIS", tag = "F") +
    lshtm_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      panel.grid.major = element_line(linewidth = 0.2),
      panel.grid.minor = element_line(linewidth = 0.2)
    )

  # --- Panel G: Heatmap of rWIS by date and location ---
  ww_rwis <- all_rwis |>
    filter(model_label == "With wastewater data") |>
    mutate(forecast_date = ymd(forecast_date))

  p_g <- ggplot(ww_rwis, aes(
    x = forecast_date, y = location, fill = rwis
  )) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      trans = "log10", name = "rWIS"
    ) +
    labs(x = "Forecast date", y = "Location", tag = "G") +
    lshtm_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      panel.grid.major = element_line(linewidth = 0.2),
      panel.grid.minor = element_line(linewidth = 0.2)
    )

  # --- Combine all panels ---
  # Layout: row 1 = A, B, C; row 2 = D, E; row 3 = F (full width);
  # row 4 = G (full width)
  p_combined <- patchwork::wrap_plots(
    p_a, p_b, p_c,
    p_d, p_e,
    p_f,
    p_g,
    design = "AABBCC\nDDDEEE\nFFFFFF\nGGGGGG",
    heights = c(1, 1, 1.5, 1.5),
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = "Forecast score comparison across models"
    ) &
    theme(legend.position = "bottom")

  if (!is.null(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    ggsave(
      filename = file.path(save_path, "fig3_score_comparison.png"),
      plot = p_combined,
      width = 18,
      height = 14
    )
  }

  return(p_combined)
}
