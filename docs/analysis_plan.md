# Analysis plan

This document describes the analysis plan to perform a retrospective evaluation of the impact of incorporating wastewater data on the forecast performance of a semi-mechanistic renewal-based model.
Forecasts of COVID-19 hospital admissions will be generated weekly and evaluated
against a the final observed hospital admissions data for all age groups.

# Data

This analysis will use publicly available data from RKI of COVID-19 hospital admissions and wastewater concentration data at the state-level in Germany during the 2024-2025 respiratory virus season.
We will use GitHub's commit history of both repositories to generate snapshots of each data source available as of each forecast date.
Weekly snapshots of both datasets will be created every Monday.

## Hospital admissions data
Hospital admissions data are reported by age group and state as 7-day rolling sums.
We will fit and generate forecasts for all age groups, at the state-level.
We will take the difference between successive days to compute daily hospital admissions incidence.


### Addressing right-truncation
The hospital admissions data provided are indexed by the data of the positive test -- which is sometimes well before patients end up hospitalised, leading to a right-truncation problem described in Wolffram et al.

We will address this in one of two ways:
1. By correcting the data for right-truncation prior to passing it to the model (pipeline approach, generally bad)
2. Separately estimating a delay distribution for each region, and modifying the `wwinference` model to account for right-truncated data (still sort of pipelining approach, but slightly better)

## Wastewater concentration data
Data on wastewater concentrations of SARS-CoV-2 are available from individual treatment plants, indexed by sample collection date.
Each wastewater treatment plant is contained within a state, and contains the metadata on the catchment area (population size) served by the wastewater treatment plant.
We will assume all individuals served by the wastewater treatment plant reside within the state.
We will jointly fit to all wastewater treatment plants within a state.

# Models
- `wwinference` (ww + hosp): This model will jointly fit to the daily hospital admissions at the state-level and the wastewater concentration data from the treatment plants within the state.
- `wwinference` (hosp only): This model will be fit only to the daily hospital admissions at the state-level
- baseline timeseries model (hosp only): This model will fit only to the daily hospital admissions at the state-level, using a time-series approach to generate forecasts

# Forecasts
Forecasts will be generated from each of the 3 models for all location-forecast dates where wastewater data is available.
Forecasts will be generated at a daily temporal resoltuion, weekly, for 28 days ahead of the final forecast dates, from October 2024 to March 2025.

Visual comparisons of forecasts overlaid with evaluation data will be used to assess the forecast performance of each model.

## Evaluation
The forecast performance will be evaluated against the final hospital admissions dataset available in June of 2025.
Posterior samples of forecasts will be log-transformed, along with the observations the models are evaluated against.
Forecasts will be scored using proper scoring rules, with metrics including the CRPS, coverage, and bias.

We will summarise and analyse scores at the level of each individual forecast problem (a specific forecast date-location combination).
This means we will summarise across the 28 horizon days to get an absolute score for each forecast.


We will then present the results by analysing the distributions of absolute scores for each forecast in the following analyses (to be made into figures):
- CRPS by model summarised across locations and forecast dates, broken down by under/over prediction and dispersion, in the form of a horizontal bar chart for each of the models
- CRPS by model summarised across forecast dates, stratified by location (heatmap x-axis = model, y-axis = location)
- CRPS by model summarised across locations, stratified by forecast date (line plot x-axis = date, y-axis = CRPS, color = model)
- distribution of relative CRPS scores compared to 1. baseline time series and 2. wwinference (hosp only) (histograms, colored by model)

The goal of these analyses and the resulting figures will be to assess:
1. How does incorporating wastewater impact the forecast performance of the semi-mechanistic renewal model?
2. How does the forecast performance of this type of model compare to a baseline time-series approach?
