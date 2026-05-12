#' LSHTM custom theme for plots
#'
#' A custom ggplot2 theme function using LSHTM branding colours and styling
#' for consistent plot appearance across the package. The theme uses LSHTM's
#' primary colour (#01454F) for borders, text, and strip backgrounds.
#'
#' @author Ciara Judge (theme design)
#'
#' @return A ggplot2 theme object
#' @importFrom ggplot2 theme element_text element_line element_rect
#' @export
#' @autoglobal
lshtm_theme <- function() {
  lshtm_theme <- theme(
    # add border 1)
    panel.border = element_rect(colour = "#01454F", fill = NA, linewidth = 0.5),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x =
      element_line(colour = "steelblue", linetype = 3, linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "aliceblue"),
    panel.grid.major.y =
      element_line(colour = "steelblue", linetype = 3, linewidth = 0.5),
    panel.grid.minor.y = element_line(colour = "aliceblue"),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#01454F"),
    axis.title = element_text(colour = "#01454F"),
    axis.ticks = element_line(colour = "#01454F"),
    # legend at the bottom 6)
    legend.position = "bottom",
    strip.text.x = element_text(colour = "white"),
    strip.text.y = element_text(colour = "white"),
    strip.background = element_rect(
      color = "#01454F", fill = "#01454F", linewidth = 1.5, linetype = "solid"
    ),
    legend.title = element_text(colour = "#01454F", face = "bold"),
    legend.text = element_text(colour = "#01454F")
  )
  return(lshtm_theme)
}

#' Add human-readable model labels
#'
#' Converts `model` and `include_ww` columns into a single `model_label`
#' column with descriptive names used in figures.
#'
#' @param df Data.frame with columns `model` and `include_ww`
#' @return The input data.frame with an additional `model_label` column
#' @importFrom dplyr mutate case_when
#' @importFrom glue glue
#' @export
#' @autoglobal
add_model_labels <- function(df) {
  result <- df |>
    mutate(
      model_label = case_when(
        model == "arima_baseline" ~ "ARIMA baseline",
        model == "wwinference" & include_ww ~ "With wastewater data",
        model == "wwinference" & !include_ww ~ "Without wastewater data",
        TRUE ~ glue("{model}-{include_ww}")
      )
    )
  return(result)
}

#' Colour and fill scales for model_ww aesthetic
#'
#' Returns a list of \code{scale_color_manual} and \code{scale_fill_manual}
#' layers that map the internal \code{model_ww} keys used in forecast data to
#' the Okabe-Ito palette and human-readable labels.
#'
#' @return A list of two ggplot2 scale objects
#' @importFrom ggplot2 scale_color_manual scale_fill_manual guide_legend
#' @keywords internal
model_ww_color_scales <- function() {
  vals <- c(
    arima_baseline = "gray",
    "wwinference-TRUE" = "#0072B2",
    "wwinference-FALSE" = "#D55E00"
  )
  lbls <- c(
    arima_baseline = "ARIMA baseline",
    "wwinference-TRUE" = "With wastewater data",
    "wwinference-FALSE" = "Without wastewater data"
  )
  list(
    scale_color_manual(
      values = vals,
      labels = lbls,
      name = "Model",
      guide = guide_legend(order = 1)
    ),
    scale_fill_manual(
      values = vals,
      labels = lbls,
      name = "Model",
      guide = guide_legend(order = 1)
    )
  )
}

#' Standard forecast ribbon layers
#'
#' Adds geom_line (median) and two geom_ribbon layers (50 % and 90 % intervals)
#' for hospital forecast data keyed by \code{model_ww}.
#'
#' @param forecast_data Data frame with columns \code{date_parsed},
#'   \code{q_0.5}, \code{q_0.25}, \code{q_0.75}, \code{q_0.05}, \code{q_0.95},
#'   \code{forecast_date_model_ww}, and \code{model_ww}.
#' @return A list of ggplot2 geom layers
#' @importFrom ggplot2 aes geom_line geom_ribbon
#' @keywords internal
#' @autoglobal
forecast_ribbon_layers <- function(forecast_data) {
  list(
    geom_line(
      data = forecast_data,
      aes(
        x = date_parsed, y = q_0.5,
        group = forecast_date_model_ww, color = model_ww
      )
    ),
    geom_ribbon(
      data = forecast_data,
      aes(
        x = date_parsed, ymin = q_0.25, ymax = q_0.75,
        group = forecast_date_model_ww, fill = model_ww
      ),
      alpha = 0.4
    ),
    geom_ribbon(
      data = forecast_data,
      aes(
        x = date_parsed, ymin = q_0.05, ymax = q_0.95,
        group = forecast_date_model_ww, fill = model_ww
      ),
      alpha = 0.3
    )
  )
}

#' Pivot quantile data from long to wide
#'
#' Convenience wrapper around \code{pivot_wider} that spreads
#' \code{quantile_level} into columns prefixed with \code{q_}.
#'
#' @param df Data frame with \code{quantile_level} and \code{predicted} columns
#' @return Data frame with one column per quantile level
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @autoglobal
pivot_quantiles <- function(df) {
  pivot_wider(df,
    names_from = quantile_level,
    values_from = predicted,
    names_prefix = "q_"
  )
}
