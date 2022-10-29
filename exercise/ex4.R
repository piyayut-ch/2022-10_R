library(tidyverse)
library(economiccomplexity)
library(gravity)
library(modelsummary)
library(flextable)


path_baci <- here::here("data/baci_extra.rds")
baci_extra <- readRDS(path_baci)

path_baci_i <- here::here("data/baci_extra_i.rds")
baci_extra_i <- readRDS(path_baci_i)

path_gravity <- here::here("data/gravity_0019.rds")
gravity_0019 <- readRDS(path_gravity)

# additional data
country_codes <- baci_extra_i |> distinct(i, i_iso3c, i_name, i_region)
product_codes <- baci_extra_i |> distinct(k, k_name, k_sector)


gravity_18_nozeros <- gravity_0019 |>
  filter(year == 2018) |>
  drop_na() |>
  filter(tradeflow_baci != 0)
gravity_18_nozeros |> head()


# Question 1
# step0: define growth function
growth_ols <- function(vec) {
  y <- vec
  x <- seq_along(y)
  result <- lm(log(y) ~ x)
  (exp(coef(result))[['x']]) - 1
}

# step1: filter data of interest
df <- baci_extra |>
  filter(t %in% ..., i_iso3c == ..., k == ...)

# step2: make a wider table where year represent years where we want to keep country code, name and region.
df_wide <- df |>
  pivot_wider(
    c(..., ..., ...),
    names_from = "...",
    values_from = "..."
  ) |>
  arrange(desc(`2015`))
df_wide

# step3: compute summarize metrics where growth in 100%
df_sum <- df |>
  group_by(...) |>
  summarize(
    avg_1620 = ...,
    growth_1620 = ...
  )
df_sum

# step4: join two tables
df_combine <- df_wide |>
  left_join(df_sum)
df_combine

# step5: save to xlsx file: optional
df_combine |> writexl::write_xlsx(here("output/ex_4-1_growth.xlsx"))


# Question 2
# step1: prepare data using average of 2016-2020
baci_1620 <- baci_extra_i |>
  filter(t %in% ...) |>
  group_by(country = ..., product = ...) |>
  summarize(value = mean(..., na.rm = TRUE)) |> ungroup()

# step2: compute balassa index continuous version
bi_dec <- balassa_index(baci_1620, discrete = FALSE)
bi_dec[1:5, ...]

# step3: extract information only rubber product
rca_rubber <- bi_dec[, ...] |>
  as.data.frame() |>
  rownames_to_column() |>
  as_tibble() |>
  select(i_iso3c = 1, rca = 2)|>
  left_join(country_codes, by = c("i_iso3c" = "iso3c")) |>
  select(i_iso3c, country_name, region, rca) |>
  arrange(-rca)

# step4: save as excel file: optional
rca_rubber |> writexl::write_xlsx(here::here("output/ex_4-2_rca_rubber.xlsx"))


# Question 3
# step1: estimate model 1
fit_ols1 <- lm(
  ... ~ ...,
  data = gravity_18_nozeros
)
summary(fit_ols1)

# step2: estimate model 2
fit_ols2 <- lm(
  ... ~ ...,
  data = gravity_18_nozeros
)
summary(fit_ols2)

# step3: compare and save
mod <- list(..., ...)
modelsummary(mod)
# optional
modelsummary(mod, output = here::here("output/ex_4-3_gravity_ols_table.docx"))