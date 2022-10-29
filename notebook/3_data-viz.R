## -----------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(here)
library(treemapify)
library(gganimate)
library(rnaturalearth)
library(sf)
library(plotly)
library(leaflet)
library(viridis)
# make usre you install.packages("gifski") for gganimate


## -----------------------------------------------------------------------------
# bilateral trade data from 2011 to 2020 using HS92 at 2 digit level
baci_extra <- readRDS(here("data/baci_extra.rds"))
baci_extra

# trade data of exporter from 2011 to 2020 using HS92 at 2 digit level
baci_extra_i <- readRDS(here("data/baci_extra_i.rds"))
baci_extra_i


## -----------------------------------------------------------------------------
## ggplot(
##   data = <DATA>, # required
##   aes(x = <VARIABLE>, y = <VARIABLE>, ...) # required
## ) +
##   geom_*(<ARGS>) +
##   geom_*(<ARGS>) +
##   ...
## 
## # or
## <DATA> |> # required
##   ggplot(aes(x = <VARIABLE>, y = <VARIABLE>, ...)) + # required
##   geom_*(<ARGS>) +
##   geom_*(<ARGS>) +
##   ...


## -----------------------------------------------------------------------------
# data prep
data_test <- baci_extra_i |>
  filter(k %in% c("08", "10", "12"), t %in% 2015:2019) |>
  mutate(gdp = gdp / 1e6)
data_test


## -----------------------------------------------------------------------------
g <- ggplot(data_test, aes(x = gdp, y = v, color = k_name))
g


## -----------------------------------------------------------------------------
g + geom_point()


## -----------------------------------------------------------------------------
g + geom_point(alpha = 0.2, size = 3)


## -----------------------------------------------------------------------------
g + 
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma)


## -----------------------------------------------------------------------------
g + 
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  stat_smooth(method = "lm", se = FALSE)


## -----------------------------------------------------------------------------
g + 
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x = "GDP (mil USD)",
    y = "Export Value (mil USD)",
    title = "Export Value of 3 Agricultural Products from 2011-2020",
    caption = "Data: BACI",
    color = "Product"
  )


## -----------------------------------------------------------------------------
g +
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x = "GDP (mil USD)",
    y = "Export Value (mil USD)",
    title = "Export Value of 3 Agricultural Products from 2011-2020",
    caption = "Data: BACI",
    color = "Product"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
g + 
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x = "GDP (mil USD)",
    y = "Export Value (mil USD)",
    title = "Export Value of 3 Agricultural Products from 2011-2020",
    caption = "Data: BACI",
    color = "Product"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_flip()


## -----------------------------------------------------------------------------
g + 
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x = "GDP (mil USD)",
    y = "Export Value (mil USD)",
    title = "Export Value of 3 Agricultural Products from 2011-2020",
    caption = "Data: BACI",
    color = "Product"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Product")) +
  facet_wrap( ~ k_name)


## -----------------------------------------------------------------------------
g + 
  geom_point(alpha = 0.2, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x = "GDP (mil USD)",
    y = "Export Value (mil USD)",
    title = "Export Value of 3 Agricultural Products from 2011-2020",
    caption = "Data: BACI",
    color = "Product"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Product")) +
  facet_wrap( ~ k_name)

ggsave(here("output/plot_test.png"), width = 8, height = 5)


## -----------------------------------------------------------------------------
data_bar <- baci_extra_i |>
  filter(t == 2020 & k == "10") |>
  mutate(i_name = fct_reorder(i_name, v)) |> #reorder country name using factor
  arrange(desc(v)) |>
  head(20)

ggplot(data_bar, aes(x = v, y = i_name, fill = i_region)) +
  geom_col() +
  scale_x_continuous(labels = comma)


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
data_line <- baci_extra_i |>
  filter(i_iso3c %in% c("THA", "VNM", "CHN") & k == "08")

ggplot(data_line, aes(x = t, y = v, color = i_name)) +
  geom_line() +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
data_area <- baci_extra_i |>
  filter(i_iso3c == "LAO") |>
  group_by(t, k_sector) |>
  summarize(v = sum(v, na.rm = TRUE)) |>
  ungroup() |>
  group_by(t) |>
  mutate(pct = v / sum(v))

# unit
ggplot(data_area, aes(x = t, y = v, fill = k_sector)) +
  geom_area(alpha = 0.8, size = 0.5, color = "black") +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)

# share
ggplot(data_area, aes(x = t, y = pct, fill = k_sector)) +
  geom_area(alpha = 0.8, size = 0.5, color = "black") +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = comma)


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
data_hist <- baci_extra |>
  filter(t == 2020, k == "10")

ggplot(data_hist, aes(x = v)) + 
  geom_histogram(
    bins = 30, 
    fill = "navyblue", 
    color = "white"
  )


## -----------------------------------------------------------------------------
ggplot(data_hist, aes(x = log(v))) +
  geom_histogram(
    bins = 30,
    fill = "navyblue",
    color = "white"
  )


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
data_boxplot <- baci_extra |>
  filter(i_iso3c %in% c("THA", "VNM", "CHN", "IND", "PAK"), k == "10", t == 2020)

ggplot(data_boxplot, aes(x = i_name, y = log(v), fill = i_name)) + 
    geom_boxplot() +
    coord_flip()


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
data_treemap <- baci_extra_i |>
  filter(t == 2020, i_iso3c == "LAO")

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


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
data_scatter <- baci_extra_i |>
  filter(t == 2019, k == "10") |>
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


## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
# load map
world <- ne_countries(scale = "small", returnclass = "sf")
class(world)


## -----------------------------------------------------------------------------
data_map <- baci_extra_i |> filter(t == 2020, k == "17")

world_baci <- world |> 
  filter(continent != "Antarctica") |>
  left_join(
    data_map,
    by = c("adm0_a3" = "i_iso3c")
  )

ggplot(world_baci, aes(fill = v)) +
  geom_sf() +
  scale_fill_viridis(option = "magma", begin = 0.2, end = 1, alpha = 1) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      title = "Export Value",
      barwidth = 30,
      direction = "horizontal"
    )
  )


## -----------------------------------------------------------------------------
data_animate <- baci_extra_i |>
  filter(k == "10") |>
  group_by(t) |>
  mutate(rank = rank(-v)) |>
  group_by(i_name) |>
  filter(rank <= 15) |>
  ungroup()

barplot <- ggplot(data_animate,
                  aes(rank, group = i_name,
                      fill = as.factor(i_region),
                      color = as.factor(i_region))) +
  geom_tile(aes(y = v/2,
                height = v,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(i_name, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=20, hjust=0.5, face="bold"),
        plot.subtitle=element_text(size=16, hjust=0.5),
        plot.caption =element_text(size=10, hjust=1, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 4, "cm"))

anim <- barplot +
  transition_states(t, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Export Value : {closest_state}',
       subtitle  =  "Top 15 Cereal Exporters",
       caption  = "Export Values in Millions USD | Data Source: BACI")

animate(anim, 200, fps = 20, height = 6, width = 8, units = "in", res = 300,
        renderer = gifski_renderer(here("output/bar_race.gif")))


## -----------------------------------------------------------------------------
data_boxplot <- baci_extra |>
  filter(i_iso3c %in% c("THA", "VNM", "CHN", "IND", "PAK"), k == "10", t == 2020)

g <- ggplot(data_boxplot, aes(x = i_name, y = log(v), fill = i_name)) + 
    geom_boxplot() +
    coord_flip()

plotly::ggplotly(g)


## -----------------------------------------------------------------------------
pal <- colorBin("YlOrRd", domain = world_baci$v, reverse = FALSE)
labels <- sprintf(
  "<strong>%s</strong><br/>%s million USD",
  world_baci$i_name, prettyNum(world_baci$v, big.mark = ",")
) |> lapply(htmltools::HTML)

leaflet(world_baci) |>
  addProviderTiles(providers$CartoDB.Positron)|> 
  addPolygons(
    fillColor = ~pal(v),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )

