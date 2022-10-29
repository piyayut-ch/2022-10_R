## -----------------------------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)


## -----------------------------------------------------------------------------
baci_2019 <- readRDS(here("data/baci_hs92_2019_2digit.rds"))
baci_2019


## -----------------------------------------------------------------------------
country_data <- read_excel(here("data/country_data.xlsx"))
country_data


## -----------------------------------------------------------------------------
country_codes <- read_excel(here("data/baci_meta.xlsx"), 1)
country_codes


## -----------------------------------------------------------------------------
product_codes <- read_excel(here("data/baci_meta.xlsx"), 2)
product_codes


## -----------------------------------------------------------------------------
country_codes <- read_csv(here("data/country_codes.csv"), show_col_types = FALSE)
country_codes


## -----------------------------------------------------------------------------
sheets <- excel_sheets(here("data/baci_meta.xlsx"))
sheets


## -----------------------------------------------------------------------------
# sheet name
country_codes <- read_excel(here("data/baci_meta.xlsx"), "country_codes")

# sheet number
product_codes <- read_excel(here("data/baci_meta.xlsx"), 2)
product_codes

# country data using default: sheet = 1
country_data <- read_excel(here("data/country_data.xlsx"))
country_data


## -----------------------------------------------------------------------------
baci_2019 <- readRDS(here("data/baci_hs92_2019_2digit.rds"))
baci_2019


## -----------------------------------------------------------------------------
## df |>
##   select(
##     column_x, # select
##     new_column_y = column_y, ... # select with rename
##   )


## -----------------------------------------------------------------------------
baci_2019 |> select(t, i, k, value = v)


## -----------------------------------------------------------------------------
## baci_2019 |> select(...)


## -----------------------------------------------------------------------------
## df |>
##   filter(
##     logical_expression(column_x) #i.e. column_x > 10
##   )


## -----------------------------------------------------------------------------
baci_2019 |> filter(i == 764 & k == "10")


## -----------------------------------------------------------------------------
## baci_2019 |> filter(...)


## -----------------------------------------------------------------------------
## df |>
##   mutate(
##     new_x = expression,
##     ...
##   )


## -----------------------------------------------------------------------------
baci_2019 |> mutate(unitprice = v / q)


## -----------------------------------------------------------------------------
## baci_2019 |> mutate(...)


## -----------------------------------------------------------------------------
## df |>
##   arrange(column1, desc(column2),)


## -----------------------------------------------------------------------------
baci_2019 |> arrange(i, desc(v))


## -----------------------------------------------------------------------------
## country_data |> arrange(...)


## -----------------------------------------------------------------------------
## df |>
##   group_by(column1, column2, ...) |>
##   summarize(
##     new_x = FUN(column_x)
##   ) |> ungroup()


## -----------------------------------------------------------------------------
baci_2019 |>
  group_by(i, k) |>
  summarize(
    v = sum(v)
  ) |> ungroup()


## -----------------------------------------------------------------------------
## country_data |>
##   group_by(...) |>
##   summarize(...)


## -----------------------------------------------------------------------------
## df |>
##   pivot_wider(
##     id_cols = c("column1", "column2", ...),
##     names_from = c("column1", "column2", ...),
##     values_from = c("column1", "column2", ...),
##     ...
##   )


## -----------------------------------------------------------------------------
country_data |>
  pivot_wider(
    id_cols = "iso3",
    names_from = "year",
    values_from = "pop"
  )


## -----------------------------------------------------------------------------
## baci_2019 |>
##   group_by(...) |>
##   summarize(...) |>
##   pivot_wider(
##     id_cols = ...,
##     names_from = ...,
##     values_from = ...
##   )
## 
## # or
## baci_2019 |>
##   pivot_wider(
##     id_cols = ...,
##     names_from = ...,
##     values_from = ...,
##     values_fn = ...
##   )


## -----------------------------------------------------------------------------
## df |>
##   pivot_longer(
##     cols = c("column1", "column2", ...),
##     names_to = "column",
##     values_to = "column",
##     ...
##   )


## -----------------------------------------------------------------------------
country_data |>
  pivot_longer(
    cols = c("pop", "gdp", "gdpcap"), 
    names_to  = "variable",
    values_to = "value"
  )


## -----------------------------------------------------------------------------
## baci_2019 |>
##   pivot_longer(
##     cols = ...,
##     names_to = ...,
##     values_to = ...
##   )


## -----------------------------------------------------------------------------
## df1 |>
##   left_join(
##     df2,
##     by = c("df1_column" = "df2_column")
##   )
## # where df1_column and df2_column are key to join both table together


## -----------------------------------------------------------------------------
baci_2019 |>
  left_join(
    country_codes,
    by = c("i" = "iso3n")
  )


## -----------------------------------------------------------------------------
## baci_2019 |>
##   left_join(
##     ...,
##     by = ...
##   )


## -----------------------------------------------------------------------------
## # csv
## baci |> write_csv(here("output/baci.csv"))
## # excel
## country_data |> write_xlsx(here("output/country_data.xlsx"))
## # RDS
## baci |> saveRDS(here("output/baci.rds"))


## -----------------------------------------------------------------------------
# tibble
iris_tibble <- as_tibble(iris)

# class
class(iris)
class(iris_tibble)

# preview
iris
iris_tibble


## -----------------------------------------------------------------------------
# count
baci_2019 |> count(k)

# bind multiple data frame
baci_2018 <- readRDS(here("data/baci_hs92_2018_2digit.rds"))
baci_2019 <- readRDS(here("data/baci_hs92_2019_2digit.rds"))

baci <- bind_rows(baci_2018, baci_2019)
baci |> head()
baci |> tail()

# baci |> write_xlsx(here("output/baci.xlsx")) # number of rows is too large


## -----------------------------------------------------------------------------
baci_2019 |>
  filter(k == "08") |>
  group_by(i) |>
  summarize(v = sum(v, na.rm = TRUE)) |>
  ungroup() |>
  arrange(-v) |>
  mutate(
    export_value = v / 1e3,
    export_share = export_value * 100 / sum(export_value)) |>
  left_join(
    country_codes,
    by = c("i" = "iso3n")
  ) |>
  select(country_name, region, export_value, export_share)


## -----------------------------------------------------------------------------
baci <- tibble() # create an empty object to hold data
for (i in 2011:2020) {
  path <- paste0("data/baci_hs92_", i, "_2digit.rds") # define path
  df <- readRDS(here(path)) # read data of each year
  df <- df |> mutate(v = v/1e3) # convert to million USD
  baci <- bind_rows(baci, df) # incremental combine data
}
baci


## -----------------------------------------------------------------------------
baci_extra <- baci |>
  left_join(
    product_codes |> select(hs_product_2digit_code:sector),
    by = c("k" = "hs_product_2digit_code")
  ) |>
  rename(
    k_name = hs_product_2digit_name,
    k_sector = sector
  ) |>
  left_join(
    country_codes |> select(iso3n, iso3c, country_name, region),
    by = c("i" = "iso3n")
  ) |>
  rename(
    i_iso3c = iso3c,
    i_name = country_name,
    i_region = region
  ) |>
  left_join(
    country_codes |> select(iso3n, iso3c, country_name, region),
    by = c("j" = "iso3n")
  ) |>
  rename(
    j_iso3c = iso3c,
    j_name = country_name,
    j_region = region
  ) |>
  select(t, i, i_iso3c:i_region, j, j_iso3c:j_region, k, k_name:k_sector, v, q)
baci_extra


## -----------------------------------------------------------------------------
baci_extra_i <- baci_extra |>
  group_by(t, i, i_iso3c, i_name, i_region, k, k_name, k_sector) |>
  summarize(
    v = sum(v, na.rm = TRUE),
    q = sum(q, na.rm = TRUE)
  ) |> ungroup() |>
  left_join(country_data, by = c("t" = "year", "i_iso3c" = "iso3"))
baci_extra_i


## -----------------------------------------------------------------------------
## baci_extra |> saveRDS(here("data/baci_extra.rds"))
## baci_extra_i |> saveRDS(here("data/baci_extra_i.rds"))

