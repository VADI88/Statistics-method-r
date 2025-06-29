# 0.0 Importing library / data ----
#
#

library(tidyverse)
library(fs)
library(broom)
library(janitor)

# 1.0 Reading the data ----
data_path <- fs_path('data')

houses_tbl <- read_csv(data_path / 'house_prices.csv')

houses_tbl <- houses_tbl |>
  clean_names()

# 2.0 T-tests to compare the sale prices of a rooftops  ----
houses_tbl |>
  tabyl(roof_style)

## comparing the price of  Gable and Hip since they are used most roofstyle
##

selected_roofstype_tbl <- houses_tbl |>
  select(roof_style, sale_price) |>
  filter(roof_style %in% c('Gable', 'Hip'))


selected_roofstype_tbl |>
  group_by(roof_style) |>
  summarise(avg_sale_price = sale_price |> mean())


selected_roofstype_tbl |>
  group_by(roof_style) |>
  nest() |>
  mutate(shapiro = map(data, ~ tidy(shapiro.test(.x$sale_price)))) |>
  unnest(shapiro)


var.test(sale_price ~ roof_style, data = selected_roofstype_tbl) |> tidy()

# A tibble: 1 × 9
# estimate num.df den.df statistic  p.value conf.low conf.high method                          alternative
# <dbl>  <int>  <int>     <dbl>    <dbl>    <dbl>     <dbl> <chr>                           <chr>
#     1    0.354   1140    285     0.354 4.55e-34    0.293     0.423 F test to compare two variances two.sided

t.test(sale_price ~ roof_style, data = selected_roofstype_tbl) |> tidy()

# # A tibble: 1 × 10
# estimate estimate1 estimate2 statistic  p.value parameter conf.low conf.high method          alternative
# <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl> <chr>           <chr>
#     1  -47393.   171484.   218877.     -6.89 2.81e-11      337.  -60930.   -33856. Welch Two Samp… two.sided

# 3.0 Anova of sale price for building type ----

houses_tbl |>
  tabyl(bldg_type)


anova_bldg <- aov(sale_price ~ bldg_type, data = houses_tbl)

anova_bldg |> tidy()

## There is difference between building type
# A tibble: 2 × 6 --
# term         df   sumsq       meansq statistic   p.value
# <chr>     <dbl>   <dbl>        <dbl>     <dbl>     <dbl>
#     1 bldg_type     4 3.18e11 79496564405.      13.0  2.06e-10
# 2 Residuals  1455 8.89e12  6109914142.      NA   NA
#
#
#

tukey_bldg <- TukeyHSD(anova_bldg)

plot(tukey_bldg)


plot(anova_bldg, 1) # Check homogeneity of variances
plot(anova_bldg, 2) # Check normality of residuals


ggplot(
  houses_tbl, # Compare densities
  aes(x = sale_price, col = bldg_type)
) +
  geom_density(linewidth = 1) +
  labs(
    title = "Density Plot of Sale Prices by Building Type",
    x = "Sale Price",
    y = "Density"
  ) +
  theme_minimal()


# 4.0 Chi squared ----

houses_tbl |>

  tabyl(garage_type)

## Only Attchd and Detchd have enough data
##

house_bldg_garage <- houses_tbl |>
  filter(garage_type %in% c('Attchd', 'Detchd')) |>
  tabyl(bldg_type, garage_type)


chi_square_test_gt <- chisq.test(house_bldg_garage)

chi_square_test_gt |> tidy()


chi_square_test_gt$expected
