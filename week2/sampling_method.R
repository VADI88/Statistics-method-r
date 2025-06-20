library(dplyr)
library(janitor)

starwars |>
  head()

set.seed(984362)


# 1.0 Simple sampling without replacement  ----
starwars_srwor <- starwars |>
  sample_n(size = 10, replace = FALSE)


## 1.1 Simple sampling with replacement ----
#

starwars_srwr <- starwars |>
  sample_n(size = 10, replace = TRUE)
starwars_srwr

## 1.2 Larger sample than population ----

starwars_srswr2 <- starwars |> # Larger sample than population
  sample_n(size = 200, replace = TRUE)


starwars |>
  summarise(avg_height = mean(height, na.rm = TRUE, digits = 2))


starwars_srswr2 |>
  summarise(avg_height = mean(height, na.rm = TRUE))


## 2 Systematic sampling
##

starwars_syst <- starwars %>%
  slice(seq(
    sample(1:5, 1), # Random starting point
    nrow(starwars),
    by = 5
  ))
starwars_syst


starwars |>
  tabyl(sex) |>
  adorn_totals("row") |>
  adorn_pct_formatting(digits = 2) |>
  select(-valid_percent)

## 3 Stratified sampling

starwars_strat <- starwars |>
  group_by(sex) |>
  sample_frac(size = 0.3)


# 4 cluster sampling
starwars |>
  tabyl(homeworld) |>
  adorn_totals("row") |>
  adorn_pct_formatting(digits = 2) |>
  select(-valid_percent)
