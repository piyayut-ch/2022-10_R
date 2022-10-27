# load libraries
library(tidyverse)
library(scales)
library(here)
library(treemapify)
library(gganimate)
library(rnaturalearth)
library(sf)
library(plotly)
library(leaflet)

# load data
baci_extra <- readRDS(here("data/baci_extra.rds"))
baci_extra_i <- readRDS(here("data/baci_extra_i.rds"))


# ex: Top 20 exporter of Meat in 2018.
data_bar <- baci_extra_i |>
  filter(t == 2018 & k == "02") |>
  mutate(i_name = fct_reorder(i_name, v)) |>
  arrange(desc(v)) |>
  head(20)

ggplot(data_bar, aes(x = v, y = i_name)) +
  geom_col(fill = "navyblue") +
  scale_x_continuous(labels = comma)


# ex: Flour Export of Thailand, Viet Nam and China from 2011 - 2020.
data_line <- baci_extra_i |>
  filter(i_iso3c %in% c("THA", "VNM", "CHN") & k == "11")

ggplot(data_line, aes(x = t, y = v, color = i_name)) +
  geom_line() +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)


# Export structure of Bangladesh from 2011-2020.
data_area <- baci_extra_i |>
  filter(i_iso3c == "BGD") |>
  group_by(t, k_sector) |>
  summarize(v = sum(v, na.rm = TRUE)) |>
  ungroup() |>
  group_by(t) |>
  mutate(pct = v / sum(v))

ggplot(data_area, aes(x = t, y = v, fill = k_sector)) +
  geom_area(alpha = 0.8, size = 0.5, color = "black") +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)

ggplot(data_area, aes(x = t, y = pct, fill = k_sector)) +
  geom_area(alpha = 0.8, size = 0.5, color = "black") +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)

# ex: Distribution of bilateral trade of oil seeds and oleaginous fruits in 2020 in log.
data_hist <- baci_extra |>
  filter(t == 2020, k == "12")

ggplot(data_hist, aes(x = log(v))) +
  geom_histogram(
    bins = 30,
    fill = "navyblue",
    color = "white"
  )


# ex: Distribution of bilateral trade of fish by selected exporters in 2020.
data_boxplot <- baci_extra |>
  filter(i_iso3c %in% c("THA", "VNM", "CHN", "IND", "PAK"), k == "03", t == 2020)

ggplot(data_boxplot, aes(x = i_name, y = log(v), fill = i_name)) + 
  geom_boxplot() +
  coord_flip()


# ex: Export structure of Cambodia in 2011.
data_treemap <- baci_extra_i |>
  filter(t == 2011, i_iso3c == "KHM")

ggplot(
  data_treemap,
  aes(area = v,
      fill = k_sector,
      subgroup = k_sector,
      label = k_name)
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


# Unit price and export value of sugar and candy in 2015.
data_scatter <- baci_extra_i |>
  filter(t == 2015, k == "17") |>
  mutate(
    unit_price = v * 1e6 / q,
    gdp = gdp / 1e6
  )

ggplot(
  data_scatter,
  aes(x = v,
      y = unit_price,
      size = gdp,
      color = i_region)
  ) +
  geom_point(alpha = 0.8) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  scale_size(range = c(1, 20), guide = NULL)
