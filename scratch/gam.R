library(mgcv)
library(targets)
library(dplyr)
library(tidyr)
library(lubridate)

tar_load(score_hosp_quantiles)

## Join ww site metadata ##

tar_load(ww_data)

ww_data_gam <- ww_data |>
  group_by(location_abbr, location_name, site, site_pop, forecast_date) |>
  summarise(
    sampling_freq = mean(diff(date)), # This means doing mean twice
    last_sample = max(date)
  ) |>
  group_by(location_abbr, location_name) |>
  summarise(
    n_sites = length(unique(site)), # Number of wastewater sites per location
    pop_cov = sum(unique(site_pop)), # There will be an issue with for Berlin
    # - need to find total popns instead.
    latency_med = median(ymd(forecast_date) - last_sample),
    latency_mean = mean(ymd(forecast_date) - last_sample),
    latency_min = min(ymd(forecast_date) - last_sample),
    sampling_freq = mean(sampling_freq)
  )

## Add scores

score_gam <- score_hosp_quantiles |>
  select(model, include_ww, location, forecast_date, wis, date) |>
  filter(model == "wwinference") |>
  rename(location_name = location)

# Scoring at 28_day time horizon
score_gam <- score_gam |>
  group_by(location_name, forecast_date, include_ww) |>
  filter(date == max(date))

# Pivot_wider to get hosp only and with ww wis
score_gam <- score_gam |>
  mutate(include_ww = if_else(include_ww, "with_ww", "hosp_only")) |>
  pivot_wider(
    id_cols = c("location_name", "forecast_date"),
    names_from = include_ww,
    values_from = wis
  )

ww_data_gam <- right_join(ww_data_gam, score_gam, by = "location_name")

gamm(
  with_ww ~ s(hosp_only) +
    s(n_sites) +
    s(pop_cov) + # normalised
    s(sampling_freq) + # make numeric
    s(latency), # maybe bs=cc here if dependent on time of year??
  data = ww_data_gam
)
# Need to add: variability in ww data (from upper and lower bounds)
