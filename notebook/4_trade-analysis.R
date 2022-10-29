## -----------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(here)
library(economiccomplexity)
library(gravity)
library(modelsummary)
library(flextable)


## -----------------------------------------------------------------------------
# bilateral trade data from 2011 to 2020 using HS92 at 2 digit level
baci_extra <- readRDS(here("data/baci_extra.rds"))
baci_extra

# trade data of exporter from 2011 to 2020 using HS92 at 2 digit level
baci_extra_i <- readRDS(here("data/baci_extra_i.rds"))
baci_extra_i

# data for gravity model
gravity_0019 <- readRDS(here("data/gravity_0019.rds"))
gravity_0019

# additional data
country_codes <- baci_extra_i |> distinct(i, i_iso3c, i_name, i_region)
country_codes

product_codes <- baci_extra_i |> distinct(k, k_name, k_sector)
product_codes


## -----------------------------------------------------------------------------
set.seed(123) # make randomization reproducible
x0 <- 100
n <- 10
growth <- rep(0.05, n)
error <- rnorm(n, 0, 0.1)
x <- c(x0, (x0 * cumprod(1 + growth))*(exp(error)))
plot(x, type="l")


## -----------------------------------------------------------------------------
growth_arithmetic <- function(vec) {
  n <- length(vec)
  (vec[n]/vec[1] - 1) / (n-1)
}
growth_arithmetic(x)


## -----------------------------------------------------------------------------
growth_geometric <- function(vec) {
  n <- length(vec)
  (vec[n]/vec[1])**(1/(n-1)) - 1
}
growth_geometric(x)


## -----------------------------------------------------------------------------
growth_exponent <- function(vec) {
  n <- length(vec)
  log(vec[n]/vec[1])/(n-1)
}
growth_exponent(x)


## -----------------------------------------------------------------------------
growth_ols <- function(vec) {
  y <- vec
  x <- seq_along(y)
  result <- lm(log(y) ~ x)
  (exp(coef(result))[['x']]) - 1
}
growth_ols(x)


## -----------------------------------------------------------------------------
# define a function that simulate a time series of n periods for 1000 series
# and estimate growth for 4 methods
sim_growth <- function(n, seed = 123) {
  set.seed(seed)
  df <- tibble()
  for (i in 1:1000) {
    x0 <- 100
    growth <- rep(0.05, n)
    error <- rnorm(n, 0, 0.1)
    x <- c(x0, (x0 * cumprod(1 + growth))*(exp(error)))
    df[i, 'round'] <- i
    df[i, 'g_ari'] <- growth_arithmetic(x)
    df[i, 'g_geo'] <- growth_geometric(x)
    df[i, 'g_exp'] <- growth_exponent(x)
    df[i, 'g_ols'] <- growth_ols(x)
  }
  df
}

# make a for loop to see the difference by varying the number of periods
n <- c(3, 4, 5, 10, 20)
seed <- 123
res <- tibble()
for (i in seq_along(n)) {
  res_ <- sim_growth(n[i], seed = seed) |>
    summarize(
      across(
        g_ari:g_ols,
        .fns = list(mean = mean, sd = sd),
        .names = "{.col}_{.fn}"
      )
    )
  res_[1, 'n_period'] <- n[i]
  res <- bind_rows(res, res_)
}
res


## -----------------------------------------------------------------------------
# step1: filter data of interest
df <- baci_extra_i |>
  filter(t %in% 2016:2020, i_iso3c == "THA") |>
  arrange(t, i_iso3c, k)
df


## -----------------------------------------------------------------------------
# step2: make a wider table where year represent years
df_wide <- df |>
  pivot_wider(
    c(k, k_name, k_sector),
    names_from = "t",
    values_from = "v"
  )
df_wide


## -----------------------------------------------------------------------------
# step3: compute summarize metrics
df_sum <- df |>
  group_by(k) |>
  summarize(
    avg_1620 = mean(v, na.rm = TRUE),
    growth_1620 = growth_ols(v) * 100
  ) |> ungroup()
df_sum


## -----------------------------------------------------------------------------
# step4: join two tables
df_combine <- df_wide |>
  left_join(df_sum)
df_combine


## -----------------------------------------------------------------------------
# step5: save to xlsx file
df_combine |> write_xlsx(here("output/4-1_export_growth_tha.xlsx"))


## -----------------------------------------------------------------------------
# step1: prepare data using average of 2016-2020
baci_1620 <- baci_extra_i |>
  filter(t %in% 2016:2020) |>
  group_by(country = i_iso3c, product = k) |>
  summarize(value = mean(v, na.rm = TRUE)) |> ungroup()
baci_1620


## -----------------------------------------------------------------------------
# step2: compute balassa index
# discrete version
bi <- balassa_index(baci_1620)
bi[1:5,1:5]

# continuous version
bi_dec <- balassa_index(baci_1620, discrete = FALSE)
bi_dec[1:5,1:5]


## -----------------------------------------------------------------------------
# step3: extract information only agriculture hscode 1-24 of Thailand.
rca_tha <- bi_dec["THA", 1:24] |>
  as.data.frame() |>
  rownames_to_column() |>
  as_tibble() |>
  select(k = 1, RCA = 2)|>
  left_join(product_codes, by = c("k" = "k")) |>
  select(k, k_name, k_sector, RCA)
rca_tha


## -----------------------------------------------------------------------------
# step4: save as excel file
rca_tha |> write_xlsx(here("output/4-2_rca_tha.xlsx"))


## -----------------------------------------------------------------------------
# step1: prepare data using average of 2016-2020
baci_1620 <- baci_extra_i |>
  filter(t %in% 2016:2020) |>
  group_by(country = i_iso3c, product = k) |>
  summarize(value = mean(v, na.rm = TRUE)) |> ungroup()
baci_1620


## -----------------------------------------------------------------------------
# step2: compute balassa index discrete version
bi <- balassa_index(baci_1620)
bi[1:5, 1:5]


## -----------------------------------------------------------------------------
# step3: compute complexity measures
com_eig <- complexity_measures(bi, method = "eigenvalues")
com_eig_country <- com_eig$complexity_index_country |>
  as_tibble(rownames = NA) |>
  rownames_to_column(var = "i_iso3c") |>
  arrange(-value) |>
  left_join(country_codes, by = c("i_iso3c" = "i_iso3c")) |>
  select(i_iso3c, i_name, i_region, value)
com_eig_country


## -----------------------------------------------------------------------------
# step4: save to excel
com_eig_country |> write_xlsx(here("output/4-3_com_country.xlsx"))


## -----------------------------------------------------------------------------
# step 1: prep data
# gravity_0019 <- readRDS(here("data/gravity_0019.rds"))
gravity_18 <- gravity_0019 |>
  filter(year == 2018)
nrow(gravity_18)

gravity_18_nozeros <- gravity_18 |>
  drop_na() |>
  filter(tradeflow_baci != 0)
nrow(gravity_18_nozeros)

gravity_18_nozeros


## -----------------------------------------------------------------------------
# step 2: estimate using ols methods
## model 1: only distance
fit_ols1 <- lm(
  log(tradeflow_baci) ~ log(gdp_o) + log(gdp_d) + dist,
  data = gravity_18_nozeros
)
summary(fit_ols1)


## -----------------------------------------------------------------------------
## model 2: distance + contig
fit_ols2 <- lm(
  log(tradeflow_baci) ~ log(gdp_o) + log(gdp_d) + dist + contig,
  data = gravity_18_nozeros
)
summary(fit_ols2)


## -----------------------------------------------------------------------------
## model 3: distance + contig + rta
fit_ols3 <- lm(
  log(tradeflow_baci) ~ log(gdp_o) + log(gdp_d) + dist + contig + rta,
  data = gravity_18_nozeros
)
summary(fit_ols3)


## -----------------------------------------------------------------------------
# step 3: model summarization and export
mod <- list(fit_ols1, fit_ols2, fit_ols3) # create a list of models to compare

## html
modelsummary(mod)


## -----------------------------------------------------------------------------
## docx
modelsummary(mod, output = here("output/4-4_gravity_ols_table.docx"))


## -----------------------------------------------------------------------------
## ## text
## sink(file = here("output/4-5_gravity_ols_table.txt"))
## 
## cat(paste0(rep("#", 80), collapse="")) # add # to seperate each model
## cat("\n") # make a new line
## cat("Model 1: \n")
## summary(fit_ols1)
## cat("\n")
## 
## cat(paste0(rep("#", 80), collapse=""))
## cat("\n")
## cat("Model 2: \n")
## summary(fit_ols2)
## cat("\n")
## 
## cat(paste0(rep("#", 80), collapse=""))
## cat("\n")
## cat("Model 3: \n")
## summary(fit_ols3)
## cat("\n")
## 
## sink()

