scores <- read_csv(file.path("output", "overall_data", "scores.csv"))

scores_overall <- scores |>
  group_by(model, include_ww) |>
  summarise(wis = mean(wis)) |>
  ungroup() |>
  mutate(model_ww = glue::glue("{model}-{include_ww}"))


ggplot(scores_overall) +
  geom_bar(aes(x = model_ww, y = wis, fill = model_ww),
    stat = "identity", position = "stack"
  )

rwis <- scores_overall$wis / scores_overall$wis[2]

scores_by_date <- scores |>
  group_by(model, include_ww, forecast_date) |>
  summarise(wis = mean(wis)) |>
  ungroup() |>
  mutate(model_ww = glue::glue("{model}-{include_ww}"))

ggplot(scores_by_date) +
  geom_line(aes(x = forecast_date, y = wis, color = model_ww))

scores_by_loc <- scores |>
  group_by(model, include_ww, location) |>
  summarise(wis = mean(wis)) |>
  ungroup() |>
  mutate(model_ww = glue::glue("{model}-{include_ww}"))

ggplot(scores_by_loc) +
  geom_point(aes(x = location, y = wis, color = model_ww))
