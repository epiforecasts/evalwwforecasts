#' Prepare scores to model
#'
#' @param scores_long Long form data with scores by model and horizon
#' @param ww_metadata Metadata on wastewater at the location and forecast date
#'    level
#'
#' @returns Wide dataframe ready to be fit to model
#' @importFrom glue glue
#' @autoglobal
prep_scores_to_model <- function(scores_long,
                                 ww_metadata) {
  # Pivot scores from long to wide

  scores_joined <- scores_long |>
    mutate(model = glue("{model}_{include_ww}")) |>
    pivot_wider(
      id_cols = c(
        "location", "forecast_date", "horizon",
        "hosp_data_real_time", "flag_missing_ww"
      ),
      names_from = "model",
      values_from = "wis",
      names_prefix = "wis_"
    ) |>
    left_join(ww_metadata, by = c(
      location = "location_name",
      "forecast_date"
    )) |>
    rename(
      wis_ww    = `wis_wwinference_TRUE`,
      wis_hosp  = `wis_wwinference_FALSE`,
      wis_arima = `wis_arima_baseline_FALSE`
    ) |>
    # GAM requires complete cases on all covariates
    filter(
      !is.na(wis_ww), !is.na(wis_hosp),
      !is.na(n_sites), !is.na(pop_coverage),
      !is.na(avg_sampling_freq), !is.na(avg_latency),
      !is.na(min_latency), !is.na(avg_data_variability)
    ) |>
    mutate(horizon_weeks = ceiling((horizon) / 7))
  return(scores_joined)
}

#' Prepare scores for fitting a GAM
#'
#' Optionally z-scores the wastewater covariates, converts location to a
#' factor and forecast date to a numeric day index.
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#'
#' @returns list with the data to fit and the scaling parameters (NULL if
#'   `standardize = FALSE`)
#' @importFrom dplyr mutate
#' @importFrom stats sd
#' @autoglobal
prep_gam_data <- function(scores_to_model, standardize = FALSE) {
  data_to_fit <- scores_to_model
  scaling_params <- NULL

  if (standardize) {
    covariates <- c(
      "n_sites", "pop_coverage", "avg_sampling_freq",
      "avg_latency", "min_latency", "avg_data_variability"
    )

    # Store means and SDs for later reference
    scaling_params <- data.frame(
      variable = covariates,
      mean = sapply(covariates, function(x) {
        return(mean(data_to_fit[[x]], na.rm = TRUE))
      }),
      sd = sapply(covariates, function(x) {
        return(sd(data_to_fit[[x]], na.rm = TRUE))
      })
    )

    for (covar in covariates) {
      data_to_fit[[covar]] <- scale(data_to_fit[[covar]])[, 1]
    }
  }

  data_to_fit <- data_to_fit |>
    mutate(
      location = factor(location),
      forecast_date_num = as.numeric(forecast_date - min(forecast_date))
    )

  return(list(data = data_to_fit, scaling_params = scaling_params))
}

#' Attach scaling and date information to a fitted GAM
#'
#' @param gam_fit fitted GAM
#' @param prepped output of `prep_gam_data()`
#'
#' @returns GAM object with `standardized`, `scaling_params` and
#'   `min_forecast_date` elements
add_gam_metadata <- function(gam_fit, prepped) {
  gam_fit$standardized <- !is.null(prepped$scaling_params)
  gam_fit$scaling_params <- prepped$scaling_params
  # Store reference date so forecast_date_num can be mapped back to dates
  gam_fit$min_forecast_date <- min(prepped$data$forecast_date)
  return(gam_fit)
}

#' Fit the GAM model
#'
#' Models WIS of the wastewater model with the hospital-only WIS as an offset,
#' so that exp(intercept + smooths) is the relative WIS (WIS_ww / WIS_hosp).
#'
#' Unweighted, the intercept is exactly the arithmetic mean of the
#' (covariate-adjusted) per-forecast ratios, which is biased in favour of the
#' hospital-only model (Bracher, Lerch, Pohle and Resin, 2026). Weighting each
#' forecast by `wis_hosp` instead makes the intercept a covariate-adjusted
#' ratio of mean scores, sum(WIS_ww) / sum(WIS_hosp).
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#' @param weighted logical, whether to weight each forecast by `wis_hosp`
#' @importFrom mgcv gam
#' @returns GAM object (with scaling attributes if standardize = TRUE)
#' @autoglobal
fit_gam <- function(scores_to_model, standardize = FALSE, weighted = FALSE) {
  prepped <- prep_gam_data(scores_to_model, standardize)
  data_to_fit <- prepped$data
  # Weights are normalised to mean 1 so the dispersion stays on the same scale
  data_to_fit$gam_weight <- if (weighted) {
    data_to_fit$wis_hosp / mean(data_to_fit$wis_hosp)
  } else {
    1
  }

  gam_fit <- gam(
    wis_ww ~ offset(log(wis_hosp)) +
      s(horizon_weeks, k = 4) +
      s(location, bs = "re") +
      s(forecast_date_num, k = 20) +
      s(n_sites, k = 5) +
      s(pop_coverage, k = 5) +
      s(avg_sampling_freq, k = 5) +
      s(avg_latency, k = 5) +
      s(min_latency, k = 5) +
      s(avg_data_variability, k = 5),
    data = data_to_fit,
    weights = gam_weight,
    family = Gamma(link = "log"),
    method = "REML"
  )

  gam_fit <- add_gam_metadata(gam_fit, prepped)
  gam_fit$weighted <- weighted
  gam_fit$long_format <- FALSE

  return(gam_fit)
}

#' Fit the long-format GAM with wastewater inclusion as a covariate
#'
#' Stacks the WIS of the hospital-only and wastewater models into one
#' response. Every term has a shared smooth, describing how forecast
#' difficulty varies for both models, and a difference smooth for the
#' wastewater model (`by = include_ww`, an ordered factor), describing how
#' the effect of including wastewater varies. exp(include_ww coefficient +
#' difference smooths) is the relative WIS.
#'
#' Neither model's score appears in a denominator. In an intercept-only
#' version, exp(include_ww coefficient) is exactly the ratio of mean scores.
#' The pairing of the two scores for the same forecast is not modelled, so
#' standard errors on the difference smooths are likely conservative and
#' those on the shared smooths anti-conservative.
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#' @importFrom mgcv bam
#' @importFrom tidyr pivot_longer
#' @importFrom stats contrasts<-
#' @returns GAM object (with scaling attributes if standardize = TRUE)
#' @autoglobal
fit_gam_long <- function(scores_to_model, standardize = FALSE) {
  prepped <- prep_gam_data(scores_to_model, standardize)
  data_to_fit <- prepped$data |>
    pivot_longer(
      c(wis_ww, wis_hosp),
      names_to = "model",
      values_to = "wis"
    ) |>
    mutate(include_ww = ordered(model == "wis_ww", levels = c(FALSE, TRUE)))
  # Ordered so `by = include_ww` gives difference smooths, but treatment
  # contrasts so the parametric include_ww term is the log relative WIS
  contrasts(data_to_fit$include_ww) <- "contr.treatment"

  gam_fit <- gam(
    wis ~ include_ww +
      s(horizon_weeks, k = 4, by = include_ww) +
      s(location, bs = "re", by = include_ww) +
      s(forecast_date_num, k = 20, by = include_ww) +
      s(n_sites, k = 5, by = include_ww) +
      s(pop_coverage, k = 5, by = include_ww) +
      s(avg_sampling_freq, k = 5, by = include_ww) +
      s(avg_latency, k = 5, by = include_ww) +
      s(min_latency, k = 5, by = include_ww) +
      s(avg_data_variability, k = 5, by = include_ww),
    data = data_to_fit,
    family = Gamma(link = "log"),
    method = "REML"
  )

  gam_fit <- add_gam_metadata(gam_fit, prepped)
  gam_fit$weighted <- FALSE
  gam_fit$long_format <- TRUE

  return(gam_fit)
}

# Breaks for log-scale axes of relative WIS
rel_wis_breaks <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.25, 1.5, 2, 3)

#' Convert a GAM covariate back to its original scale
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param var Name of the covariate
#' @param x Values of the covariate on the scale used in the fit
#'
#' @returns Values on the original (unstandardized) scale
#' @autoglobal
gam_var_to_original_scale <- function(gam_fit, var, x) {
  if (isTRUE(gam_fit$standardized) &&
    var %in% gam_fit$scaling_params$variable) {
    params <- gam_fit$scaling_params[gam_fit$scaling_params$variable == var, ]
    x <- x * params$sd + params$mean
  }
  return(x)
}

#' Build a placeholder row of covariates for predicting from a GAM
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param n Number of rows
#'
#' @returns data.frame with every covariate at its median (or first level)
#' @importFrom stats median
#' @autoglobal
get_gam_newdata <- function(gam_fit, n) {
  vars <- unique(unlist(lapply(gam_fit$smooth, `[[`, "term")))
  newdata <- lapply(vars, function(v) {
    x <- gam_fit$model[[v]]
    if (is.factor(x)) {
      return(factor(levels(x)[1], levels = levels(x)))
    }
    return(median(x))
  })
  names(newdata) <- vars
  newdata <- as.data.frame(newdata)[rep(1, n), , drop = FALSE]
  newdata$wis_hosp <- 1
  return(newdata)
}

#' Linear predictor matrix for log relative WIS across values of one term
#'
#' Returns a matrix L such that `L %*% coef(gam_fit)` is the log relative WIS
#' (log WIS_ww / WIS_hosp) at each value of `var`, with all other terms at
#' their average (zero) effect:
#' - offset model (`fit_gam()`): intercept + s(var)
#' - long-format model (`fit_gam_long()`): include_ww coefficient + the
#'   wastewater difference smooth of `var`
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param var Name of the variable to vary
#' @param values Values of `var` (on the scale used in the fit)
#'
#' @returns matrix with one row per value and one column per coefficient
#' @importFrom stats predict
#' @autoglobal
get_rel_wis_lpmatrix <- function(gam_fit, var, values) {
  newdata <- get_gam_newdata(gam_fit, length(values))
  newdata[[var]] <- values

  if (isTRUE(gam_fit$long_format)) {
    ww_levels <- levels(gam_fit$model$include_ww)
    newdata_ww <- newdata
    newdata$include_ww <- factor(
      ww_levels[1],
      levels = ww_levels, ordered = TRUE
    )
    newdata_ww$include_ww <- factor(
      ww_levels[2],
      levels = ww_levels, ordered = TRUE
    )
    # Shared terms cancel in the difference, leaving the wastewater effect
    lp <- predict(gam_fit, newdata = newdata_ww, type = "lpmatrix") -
      predict(gam_fit, newdata = newdata, type = "lpmatrix")
    keep_smooth <- function(sm) {
      return(sm$term == var && sm$by == "include_ww")
    }
    keep_param <- grep("^include_ww", colnames(lp), value = TRUE)
  } else {
    lp <- predict(gam_fit, newdata = newdata, type = "lpmatrix")
    keep_smooth <- function(sm) {
      return(sm$term == var)
    }
    keep_param <- "(Intercept)"
  }

  # Zero out every coefficient except the parametric ww term and var's smooth
  keep <- colnames(lp) %in% keep_param
  for (sm in gam_fit$smooth) {
    if (keep_smooth(sm)) keep[sm$first.para:sm$last.para] <- TRUE
  }
  lp[, !keep] <- 0

  return(lp)
}

#' Average relative WIS implied by a GAM
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#'
#' @returns exp of the intercept (offset model) or of the include_ww
#'   coefficient (long-format model)
#' @importFrom stats coef
#' @autoglobal
get_gam_avg_rel_wis <- function(gam_fit) {
  term <- if (isTRUE(gam_fit$long_format)) {
    grep("^include_ww", names(coef(gam_fit)), value = TRUE)
  } else {
    "(Intercept)"
  }
  return(exp(coef(gam_fit)[[term]]))
}

#' Predict relative WIS across values of a single GAM term
#'
#' Predicts the relative WIS (WIS_ww / WIS_hosp) as `var` changes, with all
#' other terms held at their average effect. See `get_rel_wis_lpmatrix()`.
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param var Name of the variable to vary
#' @param values Values of `var` (on the scale used in the fit) to predict at
#' @param level Width of the confidence interval
#'
#' @returns data.frame of relative WIS and confidence intervals
#' @importFrom stats qnorm coef vcov
#' @autoglobal
get_relative_wis_by_term <- function(gam_fit, var, values, level = 0.95) {
  lp <- get_rel_wis_lpmatrix(gam_fit, var, values)
  fit <- as.numeric(lp %*% coef(gam_fit))
  se <- sqrt(rowSums((lp %*% vcov(gam_fit)) * lp))
  z <- qnorm(1 - (1 - level) / 2)

  return(data.frame(
    variable = var,
    value = values,
    rel_wis = exp(fit),
    lower = exp(fit - z * se),
    upper = exp(fit + z * se)
  ))
}

#' Plot relative WIS across each wastewater covariate and horizon
#'
#' Each panel shows exp(intercept + s(x)): the expected ratio of WIS with
#' wastewater to WIS without wastewater as x varies, holding other terms at
#' their average effect. Values below 1 mean wastewater improves forecasts.
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param vars Continuous variables to plot
#' @param n_grid Number of grid points per variable
#'
#' @returns ggplot
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline geom_rug
#'   facet_wrap scale_y_log10 theme_bw labs
#' @importFrom dplyr bind_rows
#' @autoglobal
plot_rel_wis_by_covariate <- function(
  gam_fit,
  vars = c(
    "horizon_weeks", "n_sites", "pop_coverage", "avg_sampling_freq",
    "avg_latency", "min_latency", "avg_data_variability"
  ),
  n_grid = 100
) {
  preds <- bind_rows(lapply(vars, function(v) {
    x <- gam_fit$model[[v]]
    grid_vals <- seq(min(x), max(x), length.out = n_grid)
    pred <- get_relative_wis_by_term(gam_fit, v, grid_vals)
    pred$value <- gam_var_to_original_scale(gam_fit, v, pred$value)
    return(pred)
  }))
  obs <- bind_rows(lapply(vars, function(v) {
    return(data.frame(
      variable = v,
      value = gam_var_to_original_scale(gam_fit, v, unique(gam_fit$model[[v]]))
    ))
  }))

  p <- ggplot(preds, aes(x = value, y = rel_wis)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_hline(
      yintercept = get_gam_avg_rel_wis(gam_fit),
      linetype = "dotted", colour = "firebrick"
    ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
    geom_line() +
    geom_rug(data = obs, aes(x = value), inherit.aes = FALSE, alpha = 0.3) +
    facet_wrap(~variable, scales = "free_x") +
    scale_y_log10(breaks = rel_wis_breaks) +
    theme_bw() +
    labs(
      x = "Covariate value (original scale)",
      y = "Relative WIS (WIS_ww \u00f7 WIS_hosp)",
      title = "Relative forecast performance across wastewater characteristics",
      subtitle = "< 1: wastewater improves forecast; red dotted = average relative WIS" # nolint
    )

  return(p)
}

#' Plot relative WIS over forecast date
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param n_grid Number of grid points
#'
#' @returns ggplot
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline
#'   scale_y_log10 theme_bw labs
#' @autoglobal
plot_rel_wis_by_time <- function(gam_fit, n_grid = 200) {
  x <- gam_fit$model$forecast_date_num
  grid_vals <- seq(min(x), max(x), length.out = n_grid)
  preds <- get_relative_wis_by_term(gam_fit, "forecast_date_num", grid_vals)
  preds$forecast_date <- gam_fit$min_forecast_date + preds$value

  p <- ggplot(preds, aes(x = forecast_date, y = rel_wis)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
    geom_line() +
    scale_y_log10(breaks = rel_wis_breaks) +
    theme_bw() +
    labs(
      x = "Forecast date",
      y = "Relative WIS (WIS_ww \u00f7 WIS_hosp)",
      title = "Relative forecast performance over time",
      subtitle = "< 1: wastewater improves forecast"
    )

  return(p)
}

#' Plot relative WIS by location (random effects)
#'
#' Shows exp(intercept + location random effect) for each location, i.e. the
#' expected relative WIS in that location with other terms at their average.
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#'
#' @returns ggplot
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_vline scale_x_log10
#'   theme_bw labs
#' @importFrom stats reorder
#' @autoglobal
plot_rel_wis_by_location <- function(gam_fit) {
  locs <- levels(gam_fit$model$location)
  preds <- get_relative_wis_by_term(
    gam_fit, "location",
    factor(locs, levels = locs)
  )

  p <- ggplot(preds, aes(
    x = rel_wis, y = reorder(value, rel_wis),
    xmin = lower, xmax = upper
  )) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_vline(
      xintercept = get_gam_avg_rel_wis(gam_fit),
      linetype = "dotted", colour = "firebrick"
    ) +
    geom_pointrange() +
    scale_x_log10(breaks = rel_wis_breaks) +
    theme_bw() +
    labs(
      x = "Relative WIS (WIS_ww \u00f7 WIS_hosp)",
      y = NULL,
      title = "Relative forecast performance by location",
      subtitle = "< 1: wastewater improves forecast; red dotted = average relative WIS" # nolint
    )

  return(p)
}

#' Estimate the effect of each covariate on relative WIS
#'
#' For each covariate, computes the multiplicative change in relative WIS
#' when moving from its lower to its upper quantile, i.e.
#' exp(s(x_upper) - s(x_lower)), with confidence intervals from the
#' linear predictor matrix. This puts the smooth effects on a common scale
#' comparable to exponentiated GLM coefficients.
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param vars Continuous variables to summarise
#' @param lower_q,upper_q Quantiles of each covariate to contrast
#' @param level Width of the confidence interval
#'
#' @returns data.frame with one row per covariate
#' @importFrom stats predict quantile qnorm coef vcov
#' @importFrom dplyr bind_rows
#' @autoglobal
get_gam_effect_sizes <- function(
  gam_fit,
  vars = c(
    "horizon_weeks", "n_sites", "pop_coverage", "avg_sampling_freq",
    "avg_latency", "min_latency", "avg_data_variability"
  ),
  lower_q = 0.1,
  upper_q = 0.9,
  level = 0.95
) {
  z <- qnorm(1 - (1 - level) / 2)

  effect_sizes <- bind_rows(lapply(vars, function(v) {
    quantiles <- quantile(
      gam_fit$model[[v]], c(lower_q, upper_q),
      names = FALSE
    )
    # Contrast of the two rows: the average relative WIS cancels
    lp <- get_rel_wis_lpmatrix(gam_fit, v, quantiles)
    contrast <- lp[2, ] - lp[1, ]
    est <- sum(contrast * coef(gam_fit))
    se <- sqrt(as.numeric(t(contrast) %*% vcov(gam_fit) %*% contrast))
    q_orig <- gam_var_to_original_scale(gam_fit, v, quantiles)
    return(data.frame(
      variable = v,
      from = q_orig[1],
      to = q_orig[2],
      ratio = exp(est),
      lower = exp(est - z * se),
      upper = exp(est + z * se)
    ))
  }))

  return(effect_sizes)
}

#' Forest plot of covariate effects on relative WIS
#'
#' @inheritParams get_gam_effect_sizes
#'
#' @returns ggplot
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_vline scale_x_log10
#'   theme_bw labs
#' @importFrom glue glue
#' @importFrom stats reorder
#' @autoglobal
plot_gam_effect_sizes <- function(gam_fit, lower_q = 0.1, upper_q = 0.9) {
  effect_sizes <- get_gam_effect_sizes(
    gam_fit,
    lower_q = lower_q, upper_q = upper_q
  )
  effect_sizes$label <- glue(
    "{effect_sizes$variable}\n",
    "({signif(effect_sizes$from, 3)} \u2192 {signif(effect_sizes$to, 3)})"
  )

  p <- ggplot(effect_sizes, aes(
    x = ratio, y = reorder(label, ratio),
    xmin = lower, xmax = upper
  )) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_pointrange() +
    scale_x_log10(breaks = rel_wis_breaks) +
    theme_bw() +
    labs(
      x = "Multiplicative change in relative WIS",
      y = NULL,
      title = "Effect of wastewater characteristics on relative WIS",
      subtitle = glue(
        "Change from {lower_q * 100}th to {upper_q * 100}th percentile; ",
        "< 1: wastewater benefit increases"
      )
    )

  return(p)
}

#' Fit the GLM model
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#' @importFrom stats glm Gamma
#' @returns GLM object (with scaling attributes if standardize = TRUE)
#' @autoglobal
fit_glm <- function(scores_to_model, standardize = TRUE) {
  prepped <- prep_gam_data(scores_to_model, standardize)

  glm_fit <- glm(
    wis_ww ~ offset(log(wis_hosp)) +
      n_sites +
      pop_coverage +
      avg_sampling_freq +
      avg_latency +
      min_latency +
      avg_data_variability,
    data = prepped$data,
    family = Gamma(link = "log")
  )
  glm_fit$standardized <- standardize
  glm_fit$scaling_params <- prepped$scaling_params

  return(glm_fit)
}

#' Table of exponentiated GLM coefficients
#'
#' @param glm_fit GLM object returned by `fit_glm()`
#' @importFrom broom tidy
#' @importFrom dplyr mutate across select
#' @importFrom gt gt fmt_number tab_header
#' @returns gt table
#' @autoglobal
get_glm_coef_table <- function(glm_fit) {
  coef_table <- tidy(glm_fit, conf.int = TRUE) |>
    mutate(across(
      c(estimate, conf.low, conf.high), exp,
      .names = "exp_{.col}"
    )) |>
    select(term, exp_estimate, exp_conf.low, exp_conf.high, p.value) |>
    gt() |>
    fmt_number(decimals = 3) |>
    tab_header(
      title = "GLM coefficients (exponentiated)",
      subtitle = paste(
        "exp(estimate) < 1: covariate associated with lower WIS_ww",
        "(better forecast)"
      )
    )

  return(coef_table)
}

#' Make a plot of the partial effects
#'
#' @param gam_fit GAM object
#' @importFrom gratia draw
#' @importFrom ggplot2 theme_bw labs
#' @returns ggplot
#' @autoglobal
partial_plot <- function(gam_fit) {
  partial_plots <- draw(gam_fit, residuals = TRUE, rug = TRUE) &
    theme_bw() &
    labs(y = "Partial effect on log(WIS_ww)")

  return(partial_plots)
}

#' Plot fitted against observed WIS
#'
#' @param gam_fit GAM object returned by `fit_gam()`
#' @param scores_to_model wide table of scores the GAM was fit to
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_abline
#'   scale_x_log10 scale_y_log10 theme_bw labs
#' @importFrom dplyr mutate
#' @importFrom stats fitted
#' @returns ggplot
#' @autoglobal
plot_scores_fit_gam <- function(gam_fit, scores_to_model) {
  p <- scores_to_model |>
    mutate(fitted = fitted(gam_fit)) |>
    ggplot(aes(x = wis_hosp, y = wis_ww)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_line(aes(y = fitted), colour = "firebrick", linewidth = 0.9) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", colour = "grey40"
    ) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() +
    labs(
      x = "WIS (hosp-only model)",
      y = "WIS (ww+hosp model)",
      title = "Fitted vs observed WIS: wastewater vs hosp-only",
      subtitle = paste(
        "Points below dashed line = wastewater model outperforms;",
        "red line = GAM fit"
      )
    )

  return(p)
}
