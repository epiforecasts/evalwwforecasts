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
      "avg_latency", "min_latency", "avg_data_variability", "state_pop"
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

  gam_fit <- bam(
    wis ~ include_ww +
      s(horizon, k = 4) + s(horizon, k = 4, by = include_ww) +
      s(location, bs = "re") + s(location, bs = "re", by = include_ww) +
      s(forecast_date_num, k = 40) +
      s(forecast_date_num, k = 40, by = include_ww) +
      # One value per state, so keep this smooth simple
      s(state_pop, k = 3) + s(state_pop, k = 3, by = include_ww) +
      s(n_sites, k = 5) + s(n_sites, k = 5, by = include_ww) +
      s(pop_coverage, k = 5) + s(pop_coverage, k = 5, by = include_ww) +
      s(avg_sampling_freq, k = 5) +
      s(avg_sampling_freq, k = 5, by = include_ww) +
      s(avg_latency, k = 5) + s(avg_latency, k = 5, by = include_ww) +
      s(min_latency, k = 5) + s(min_latency, k = 5, by = include_ww) +
      s(avg_data_variability, k = 5) +
      s(avg_data_variability, k = 5, by = include_ww),
    data = data_to_fit,
    family = Gamma(link = "log"),
    method = "fREML",
    discrete = TRUE
  )

  gam_fit <- add_gam_metadata(gam_fit, prepped)
  gam_fit$weighted <- FALSE
  gam_fit$long_format <- TRUE

  return(gam_fit)
}

#' Get plot of location specific multiplicative effect
#'
#' @param gam_fit long-format GAM fit from `fit_gam_long()`
#' @importFrom mgcv gam.check
#' @importFrom ggplot2 geom_errorbar coord_flip scale_x_discrete
#' @returns ggplot object
#' @autoglobal
get_plot_effect_by_location <- function(gam_fit) {
  # Tables and plots that I will export to separate figures
  par(mfrow = c(2, 2))
  gam.check(gam_fit)
  par(mfrow = c(1, 1))

  estimates <- smooth_estimates(gam_fit)

  make_ci_table <- function(smooth_name, group_var) {
    cis <- estimates |>
      filter(.smooth == smooth_name) |>
      select(.smooth, {{ group_var }}, .estimate, .se) |>
      rename(est = .estimate, se = .se) |>
      mutate(
        ci_lower = est - 1.96 * se,
        ci_upper = est + 1.96 * se,
        effect = exp(est),
        est_lower = exp(ci_lower),
        est_upper = exp(ci_upper),
        excludes_zero = ci_lower > 0 | ci_upper < 0
      ) |>
      arrange(effect)
    retrun(cis)
  }

  location_ci <- make_ci_table("s(location):include_wwTRUE", location)

  knitr::kable(select(location_ci, -.smooth))

  p <- ggplot(
    location_ci,
    aes(x = reorder(location, -effect), y = effect)
  ) +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.2) +
    geom_point(color = "seagreen", size = 5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    labs(
      y = "Multiplicative effect on WIS of including wastewater",
      x = "Location"
    ) +
    theme_bw() +
    coord_flip() +
    theme(
      text            = element_text(size = 27),
      axis.text.x     = element_text(size = 24),
      axis.text.y     = element_text(size = 24),
      axis.title      = element_text(size = 27),
      legend.text     = element_text(size = 24),
      legend.title    = element_text(size = 25.5),
      strip.text      = element_text(size = 25.5)
    ) +
    scale_x_discrete(labels = function(x) gsub("-", "-\n", x)) # nolint

  return(p)
}

#' Get plot of the overall multiplicative effect of including wastewater
#'
#' Plots exp(beta_ww), the exponentiated `include_ww` coefficient from the
#' long-format GAM, with a Wald confidence interval. Because the difference
#' smooths are centred, this is the relative WIS (WIS_ww / WIS_hosp) with all
#' difference smooths at zero.
#'
#' @param gam_fit long-format GAM fit from `fit_gam_long()`
#' @param level confidence level for the interval
#'
#' @returns ggplot object
#' @importFrom stats coef vcov qnorm
#' @autoglobal
get_plot_effect_ww <- function(gam_fit, level = 0.95) {
  coef_name <- "include_wwTRUE"
  est <- coef(gam_fit)[[coef_name]]
  se <- sqrt(vcov(gam_fit)[coef_name, coef_name])
  z <- qnorm(1 - (1 - level) / 2)

  ww_effect <- data.frame(
    term = "Include wastewater",
    effect = exp(est),
    est_lower = exp(est - z * se),
    est_upper = exp(est + z * se)
  )

  p <- ggplot(ww_effect, aes(x = term, y = effect)) +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.1) +
    geom_point(color = "seagreen", size = 5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    labs(
      y = "Multiplicative effect on WIS of including wastewater",
      x = NULL
    ) +
    theme_bw() +
    coord_flip() +
    theme(
      text = element_text(size = 27),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      axis.title = element_text(size = 27)
    )

  return(p)
}

#' Get plot of the multiplicative effect of wastewater characteristics
#'
#' Plots exp(difference smooth) for each wastewater characteristic from the
#' long-format GAM, i.e. how the multiplicative effect of including
#' wastewater on WIS changes across values of that characteristic. Smooths
#' are centred, so the curves are relative to the overall effect,
#' exp(beta_ww), and exclude its uncertainty.
#'
#' @param gam_fit long-format GAM fit from `fit_gam_long()`
#' @param vars character vector of wastewater characteristics to plot
#' @param level confidence level for the interval
#' @param n_grid number of points to evaluate each smooth at
#'
#' @returns ggplot object
#' @importFrom gratia smooth_estimates
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline
#'   facet_wrap scale_y_log10 labs theme_bw theme element_text
#' @importFrom stats qnorm
#' @autoglobal
get_plot_ww_chars <- function(gam_fit,
                              vars = c(
                                "n_sites", "pop_coverage", "avg_sampling_freq",
                                "avg_latency", "min_latency",
                                "avg_data_variability"
                              ),
                              level = 0.95,
                              n_grid = 100) {
  z <- qnorm(1 - (1 - level) / 2)

  smooth_ci <- bind_rows(lapply(vars, function(v) {
    est <- smooth_estimates(
      gam_fit,
      select = glue("s({v}):include_wwTRUE"),
      n = n_grid
    )
    data.frame(
      variable = v,
      x = est[[v]],
      effect = exp(est$.estimate),
      est_lower = exp(est$.estimate - z * est$.se),
      est_upper = exp(est$.estimate + z * est$.se)
    )
    return(est)
  }))

  p <- ggplot(smooth_ci, aes(x = x, y = effect)) +
    geom_ribbon(aes(ymin = est_lower, ymax = est_upper),
      fill = "seagreen", alpha = 0.3
    ) +
    geom_line(color = "seagreen", linewidth = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    facet_wrap(~variable, scales = "free_x") +
    labs(
      y = "Multiplicative effect on WIS of including wastewater",
      x = ""
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 27),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      axis.title = element_text(size = 27),
      strip.text = element_text(size = 25.5)
    )

  return(p)
}

# Breaks for log-scale axes of relative WIS, in reciprocal pairs (r, 1 / r)
# so ticks are symmetric about 1
rel_wis_breaks <- c(1 / 3, 0.5, 2 / 3, 0.8, 0.9, 1, 1 / 0.9, 1.25, 1.5, 2, 3)
rel_wis_labels <- c(
  "0.33", "0.5", "0.67", "0.8", "0.9", "1", "1.11", "1.25", "1.5", "2", "3"
)

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
    scale_x_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
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

#' Get residual diagnostic plots for a GAM
#'
#' Combines the standard `gratia::appraise()` panels (QQ plot, residuals vs
#' linear predictor, histogram, observed vs fitted) with deviance residuals
#' against forecast horizon and forecast date. For the long-format model these
#' are split by model (hospital only vs wastewater), and a final panel plots
#' the residuals of the two models for the same forecast against each other.
#' The long-format model treats these as independent, so a strong
#' correlation means its standard errors are mis-stated.
#'
#' @param gam_fit GAM fit from `fit_gam()` or `fit_gam_long()`
#'
#' @returns patchwork object
#' @importFrom gratia appraise
#' @importFrom mgcv k.check
#' @importFrom stats residuals fitted cor
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline
#'   geom_abline labs theme_bw
#' @importFrom patchwork wrap_plots
#' @autoglobal
get_plot_gam_diagnostics <- function(gam_fit) {
  # Basis dimension check: k-index well below 1 with a small p-value suggests
  # k is too low for that smooth
  print(k.check(gam_fit))

  long_format <- isTRUE(gam_fit$long_format)
  horizon_var <- intersect(c("horizon", "horizon_weeks"), names(gam_fit$model))
  resid_data <- gam_fit$model |>
    mutate(
      resid = residuals(gam_fit, type = "deviance"),
      fitted = fitted(gam_fit),
      horizon = .data[[horizon_var[1]]],
      model = if (long_format) {
        ifelse(include_ww == "TRUE", "wastewater", "hospital only")
      } else {
        "wastewater vs hospital only"
      }
    )

  p_appraise <- appraise(gam_fit, point_alpha = 0.1)

  p_horizon <- ggplot(resid_data, aes(x = horizon, y = resid, colour = model)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Horizon", y = "Deviance residual", colour = NULL) +
    theme_bw()

  p_date <- ggplot(
    resid_data,
    aes(x = forecast_date_num, y = resid, colour = model)
  ) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Days since first forecast date", y = "Deviance residual",
      colour = NULL
    ) +
    theme_bw()

  plots <- list(p_horizon, p_date)

  if (long_format) {
    paired <- resid_data |>
      select(location, forecast_date_num, horizon, model, resid) |>
      pivot_wider(names_from = model, values_from = resid)
    resid_cor <- cor(
      paired[["hospital only"]], paired[["wastewater"]],
      use = "complete.obs"
    )
    p_paired <- ggplot(
      paired,
      aes(x = .data[["hospital only"]], y = .data[["wastewater"]])
    ) +
      geom_point(alpha = 0.05) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(
        x = "Residual, hospital only", y = "Residual, wastewater",
        title = sprintf("Paired residual correlation = %.2f", resid_cor)
      ) +
      theme_bw()
    plots <- c(plots, list(p_paired))
  }

  p <- wrap_plots(p_appraise, wrap_plots(plots, nrow = 1), ncol = 1)

  return(p)
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
