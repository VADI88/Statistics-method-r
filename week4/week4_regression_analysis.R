# 0.0 Importing

library(tidymodels)
library(tidyverse)
library(skimr)
library(janitor)
library(fs)
library(broom)
library(dotwhisker)
library(performance)

data_folder_path <- fs_path('data')
houses_tbl <- read_csv(data_folder_path / 'house_prices.csv')


# 1.0 Simple regression ----
houses_tbl |>
  ggplot(aes(x = LotArea, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()


lm_model <- linear_reg() |>
  set_engine('lm')


simple_lm_fit <- lm_model |>
  fit(SalePrice ~ LotArea, data = houses_tbl)
#fit_xy(x = houses_tbl$SalePrice ,y =  houses_tbl$LotArea)

tidy(simple_lm_fit) %>%
  dwplot(
    dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
  )

simple_res <- bind_cols(
  predict(simple_lm_fit, new_data = houses_tbl |> select(LotArea)),
  houses_tbl |> select(SalePrice)
)

simple_rmse <- rmse(simple_res, truth = SalePrice, estimate = .pred)


# 2.0 Multiple Linear regression----
#

multiple_lm_fit <- lm_model |>
  fit(
    SalePrice ~
      LotArea +
        YearBuilt +
        BldgType,
    data = houses_tbl
  )

tidy(multiple_lm_fit)

tidy(multiple_lm_fit) %>%
  dwplot(
    dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
  ) +
  theme_minimal()


multiple_res <- bind_cols(
  predict(
    multiple_lm_fit,
    new_data = houses_tbl |> select(LotArea, YearBuilt, BldgType)
  ),
  houses_tbl |> select(SalePrice)
)

multiple_rmse <- rmse(multiple_res, truth = SalePrice, estimate = .pred)

car::vif(multiple_lm_fit) # Check multicollinearity

# 3.0 Ploynominal regression ----

poly_lm_fit <- lm_model |>
  fit(
    SalePrice ~ LotArea + I(LotArea^2), # Perform Polynomial Regression
    data = houses_tbl
  )


tidy(poly_lm_fit) %>%
  dwplot(
    dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
  ) +
  theme_minimal()


houses_tbl |>
  ggplot(
    # Polynomial Regression plot
    aes(x = LotArea, y = SalePrice)
  ) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  theme_minimal()


poly_res <- bind_cols(
  predict(
    poly_lm_fit,
    new_data = houses_tbl |> select(LotArea)
  ),
  houses_tbl |> select(SalePrice)
)

poly_rmse <- rmse(poly_res, truth = SalePrice, estimate = .pred)


## 4.0 GLMNET regression ----
# Receipe

houses_receipe <- houses_tbl |>
  select(LotArea, YearBuilt, BldgType, SalePrice) |>
  recipe(SalePrice ~ .) |>
  step_dummy(BldgType)


ridge_model <-
  linear_reg(penalty = tune(), mixture = 0) %>% # Ridge regression (L2)
  set_engine("glmnet")


ridge_workflow <-
  workflow() %>%
  add_model(ridge_model) %>%
  add_recipe(houses_receipe)

# 3. Define a grid for lambda
ridge_params <- extract_parameter_set_dials(ridge_model)
ridge_grid <- grid_regular(ridge_params, levels = 50) # Example: 50 levels


folds <- vfold_cv(
  houses_tbl |>
    select(LotArea, YearBuilt, BldgType, SalePrice),
  v = 5
)

ridge_results <-
  ridge_workflow %>%
  tune_grid(
    resamples = folds,
    grid = ridge_grid,
    metrics = metric_set(rmse, rsq)
  )

ridge_results |>
  select_best(metric = "rmse")


best_ridge <-
  ridge_results %>%
  select_best(metric = "rmse")


ridge_results |>
  unnest(.metrics)


final_ridge_workflow <-
  ridge_workflow %>%
  finalize_workflow(best_ridge)


# 6. Fit the finalized workflow on the full training data
final_ridge_fit <-
  final_ridge_workflow %>%
  fit(data = houses_tbl |> select(LotArea, YearBuilt, BldgType, SalePrice))


final_ridge_results <-
  final_ridge_fit %>%
  augment(houses_tbl |> select(LotArea, YearBuilt, BldgType, SalePrice)) %>%
  metrics(truth = SalePrice, estimate = .pred)


coef_data <-
  extract_fit_engine(final_ridge_fit) %>%
  tidy()

coef_rigde <- tidy(final_ridge_fit)

ridge_rmse <- final_ridge_results |>
  filter(.metric == 'rmse')

# 5.0 Comparing the models -----
#
simple_rmse |>
  mutate(models = 'simple_lm') |>
  bind_rows(multiple_rmse |> mutate(models = 'mulitple_lm')) |>
  bind_rows(poly_rmse |> mutate(models = 'poly_reg')) |>
  bind_rows(ridge_rmse |> mutate(models = 'ridge_reg'))

# 6.0 Models assumptions / diagnostics

multiple_lm_fit |>
  check_model()
