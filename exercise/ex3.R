library(tidyverse)
library(scales)
library(treemapify)
library(rnaturalearth)
library(sf)
library(viridis)

path_baci <- here::here("data/baci_extra_i.rds")
baci_extra_i <- readRDS(path_baci)

world <- ne_countries(scale = "small", returnclass = "sf")
plot(world['admin'])


# Question 1
data_bar <- baci_extra_i |>
  filter(t == ... & k == ...) |>
  mutate(i_name = fct_reorder(i_name, v)) |>
  arrange(desc(v)) |>
  head(20)

ggplot(data_bar, aes(x = ..., y = ..., fill = ...)) +
  geom_col() +
  scale_x_continuous(labels = comma)


# Question 2
data_line <- baci_extra_i |>
  filter(i_iso3c %in% c(...) & k == ...)

ggplot(data_line, aes(x = ..., y = ..., color = ...)) +
  geom_line() +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)


# Question 3
data_area <- baci_extra_i |>
  filter(i_iso3c == ...) |>
  group_by(...) |>
  summarize(v = sum(v, na.rm = TRUE)) |>
  ungroup() |>
  group_by(t) |>
  mutate(pct = ...)

ggplot(data_area, aes(x = ..., y = ..., fill = ...)) +
  geom_area(alpha = 0.8, size = 0.5, color = "black") +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)


# Question 4
data_hist <- baci_extra |>
  filter(t == ..., k == ...)

ggplot(data_hist, aes(x = ...)) +
  geom_histogram(
    bins = 30,
    fill = "navyblue",
    color = "white"
  )


# Question 5
data_boxplot <- baci_extra_i |>
  filter(k %in% c(...), t == ...)

ggplot(data_boxplot, aes(x = ..., y = log(...), fill = ...)) +
  geom_boxplot() +
  coord_flip()


# Question 6
data_treemap <- baci_extra_i |>
  filter(t == ..., i_iso3c == ...)

ggplot(
  data_treemap,
  aes(area = ...,
      fill = ...,
      subgroup = ...,
      label = ...)
  ) +
  geom_treemap(color = "white", start = "topleft") +
  geom_treemap_subgroup_border(color = "grey40", start = "topleft") +
  geom_treemap_text(
    color = "grey30",
    place = "topleft",
    start = "topleft",
    reflow = TRUE) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


# Qeustion 7
data_scatter <- baci_extra_i |>
  filter(t == ..., k == ...) |>
  mutate(
    unit_price = v * 1e6 / q,
    gdp = gdp / 1e6
  )

ggplot(
  data_scatter,
  aes(x = ...,
      y = ...,
      size = ...,
      color = ...)
  ) +
  geom_point(alpha = 0.8) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  scale_size(range = c(1, 20), guide = NULL)


# Question 8
mytheme <- theme(
  panel.background = element_rect(fill = 'black'),
  panel.grid.major = element_line(color = 'black')
)

data_map <- baci_extra_i |> filter(t == ..., k == ...)

world_baci <- world |>
  filter(continent != "Antarctica") |>
  left_join(
    data_map,
    by = c("adm0_a3" = "i_iso3c")
  )

ggplot(world_baci, aes(fill = ...)) +
  geom_sf(color = "grey60") +
  scale_fill_viridis(option = "magma", begin = 0.1, end = 1) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      title = "Export Value",
      barwidth = 30,
      direction = "horizontal"
    )
  ) +
  mytheme