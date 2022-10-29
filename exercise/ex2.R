library(tidyverse)
library(openxlsx)

path_baci <- here::here("data/baci_hs92_2019_2digit.rds")
path_metadata <- here::here("data/baci_meta.xlsx")

baci_2019 <- readRDS(path_baci)
country_codes <- read.xlsx(path_metadata, 1) |> as_tibble()
product_codes <- read.xlsx(path_metadata, 2) |> as_tibble()


# Question 1


# Question 2
daily_2019 <- baci_2019 |> filter(...)


# Question 3
daily_2019 |> mutate(unit_price = ...)


# Question 4
daily_2019 |>
  group_by(...) |>
  summarize(v = ...)


# Question 5
daily_2019 |>
  pivot_wider(
    id_cols = ...,
    names_from = ...,
    values_from = ...
  )


# Question 6
daily_2019 |>
  left_join(..., by = ...) |>
  rename() |>
  left_join(..., by = ...) |>
  rename() |>
  pivot_wider(
    id_cols = ...,
    names_from = ...,
    values_from = ...
  )