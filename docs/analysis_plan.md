# Analysis plan

This document describes the analysis plan to perform a retrospective evaluation of the impact of incorporating wastewater data on the forecast performance of a semi-mechanistic renewal-based model.
Forecasts of COVID-19 hospital admissions will be generated weekly and evaluated against the final observed hospital admissions data, across all age groups in Germany during the 2024-2025 respriatory virus season. 

# Aims

The goal of these analyses will be to assess:
1. How does incorporating wastewater impact the forecast performance of a semi-mechanistic renewal model? 
3. How do characteristics of the wastewater surveillance system impact the relative forecast performance of the wastewater-informed model?


# Data

This analysis will use publicly available data from RKI of COVID-19 hospital admissions at the state-level and wastewater concentration data at the site-level in Germany during the 2024-2025 respiratory virus season.
We will use GitHub's commit history of both repositories to generate snapshots of each data source available as of each forecast date.
Weekly snapshots of both datasets will be created every Monday from October 2024 to March 2025.

## Hospital admissions data

Hospital admissions data are reported by age group and state as 7-day rolling sums.
We will fit and generate forecasts for all age groups, at the state-level.
We will take the difference between successive days to compute daily hospital admissions incidence as an input into the model. 

### Addressing right-truncation
The hospital admissions data provided are indexed by the data of the positive test -- which is sometimes well before patients end up hospitalised, leading to a right-truncation problem described in Wolffram et al.

We will address this by separately estimating a delay distribution for each region and forecast date using `baselinenowcast`, and modifying the `wwinference` model to account for right-truncated data via incorporating a reporting delay in the observation model.

## Wastewater concentration data
Data on wastewater concentrations of SARS-CoV-2 are available from individual wastewater treatment plants (will refer to these as sites), indexed by sample collection date.
Each wastewater treatment plant is contained within a state, and contains the metadata on the catchment area (population size) served by the wastewater treatment plant.
We will assume all individuals served by the wastewater treatment plant reside within the state.
We will jointly fit to all wastewater treatment plants within a state.

# Models
- `wwinference` (ww + hosp): This model will jointly fit to the daily hospital admissions at the state-level and the wastewater concentration data from the treatment plants within the state.
- `wwinference` (hosp only): This model will be fit only to the daily hospital admissions at the state-level

In the SI, we will compare performance of both these models to a baseline auto-ARIMA model fit only to the right-truncation corrected hospital admissions data.  

# Forecasts
Forecasts will be generated from each of the models for all location-forecast dates where wastewater data is available.
Forecasts will be generated at a daily temporal resolution, each week, for a 28 day horizon ahead of the forecast date, from October 2024 to March 2025.

## Evaluation
Visual comparisons of forecasts overlaid with evaluation data will be used to assess the forecast performance of each model.

The forecast performance will be evaluated against the final hospital admissions dataset available in June of 2025.
Posterior samples of forecasts will be log-transformed, along with the observations the models are evaluated against.
Forecasts will be scored using proper scoring rules, with metrics including the CRPS, coverage, and bias.

We will typically summarise and analyse scores at the level of each individual forecast problem (a specific forecast date-location combination).
This means we will summarise across the 28 horizon days to get an absolute score for each forecas problem, unless we specifically state we are investigating performance stratified by horizon. 


We will then present the results by analysing the distributions of absolute scores for each forecast in the following analyses (to be made into figures):
- CRPS by model summarised across locations and forecast dates, broken down by under/over prediction and dispersion, in the form of a horizontal bar chart for each of the models, for 3 different horizons (7, 14, 28 day horizon).
- CRPS by model summarised across forecast dates, stratified by location (heatmap x-axis = model, y-axis = location)
- CRPS by model summarised across locations, stratified by forecast date (line plot x-axis = date, y-axis = CRPS, color = model)
- 50th and 90th interval coverage summarised across locations and forecast dates (horizontal bar chart, color = model)
- distribution of relative CRPS scores compared to wwinference (hosp only) (histograms, colored by model)

We will then perform an exploratory analysis into the influence of different conditions on the relative performance of the "ww + hosp" model compared to the "hosp only" model. 
The proposed structures of the meta-regression will be as follows:

$$
CRPS^{ww+hosp}_{t,l} \sim \beta + CRPS^{hosp}_{t,l} + s(location, bs = "re") + s(# of sites) + s(trend, bs = "re") + s(pop_cov_ww) + s(avg_freq_sampling, k = 10) + s(avg latency reporting)
$$

The idea behind this analysis will be that the CRPS score for the hospital admissions only model defines the baseline "difficulty" in the forecast problem, and each of the additional components has some (assumed to be non-linear) positive or negative impact on the forecast accuracy of the wastewater-informed model. 

We will plot the partial effects as a function of location, epidemic trend, number of sites, population coverage of wastewater surveillance, the average sampling frequency in the wastewater data and the average latency in reporting of the wastewater data. 

The estimated $\beta$ reflects the adjusted (multiplicative) impact of incorporating wastewater on forecast performance compared to forecast performance using only hospital admissions in the same model. 
We will implement this using a linear-link function in the R package `mgcv`.
These exploratory analyses will be used to generate hypotheses into the impact of these characteristics on the performance of a wastewater-informed forecast. 
