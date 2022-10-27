# load libraries
library(tidyverse)
library(here)
library(readxl)
library(writexl)

# load data
baci_2019 <- readRDS(here("data/baci_hs92_2019_2digit.rds"))
country_data <- read_excel(here("data/country_data.xlsx"))
country_codes <- read_excel(here("data/baci_meta.xlsx"), "country_codes")
product_codes <- read_excel(here("data/baci_meta.xlsx"), 2)

# ex: Select year, destination country and trade volume from `baci_2020`
# dataset where rename q.
baci_2019 |> select(t, j, "volume" = q)

# ex: Get Vegetables and Fruit import flow of India.
baci_2019 |> filter(j == "699", k == "08")

# ex: Change unit of the unit price to US dollar per ton
baci_2019 |> mutate(unitprice = v * 1000 / q)

# ex: Sort `country_data` by year ascending and population descending.
country_data |> arrange(year, desc(pop))

# ex:  Use `country_data`, find total population by year.
country_data |>
  group_by(year) |>
  summarize(pop = sum(pop, na.rm = TRUE) / 1e3)

# ex: Use `baci_2019`, make a matrix of trade value where rows are exporters 
# and columns are importers.
baci_2019 |>
  group_by(i, j) |>
  summarize(v = sum(v, na.rm = TRUE) / 1e3) |>
  pivot_wider(
    id_cols = "i",
    names_from = "j",
    values_from = "v"
  )

# or
baci_2019 |>
  pivot_wider(
    id_cols = "i",
    names_from = "j",
    values_from = "v",
    values_fn = sum
  )

# ex: Use `baci_2019` and pivot longer trade value and quantity
baci_2019 |>
  pivot_longer(
    cols = c("v", "q"),
    names_to = "variable",
    values_to = "value"
  )

# ex: Add product description to `baci_2019`
baci_2019 |>
  left_join(
    product_codes,
    by = c("k" = "hs_product_2digit_code")
  )