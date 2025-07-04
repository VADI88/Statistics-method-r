# 0.0 Importing

library(tidymodels)
library(tidyverse)
library(skimr)
library(janitor)
library(fs)
library(broom)
library(dotwhisker)
library(performance)
library(ggrepel)

data_folder_path <- fs_path('data')
forest_cover_tbl <- read_csv(data_folder_path / 'forest_cover_type.csv')

forest_cover_tbl <- forest_cover_tbl |>
  clean_names()

forest_cover_tbl |>
  glimpse()

forest_cover_tbl |>
  tabyl(cover_type)


# 1.0 LR ----
#

binary_forest_tbl <- forest_cover_tbl |>
  filter(cover_type %in% c(1, 2)) |>
  select(
    cover_type,
    elevation,
    aspect,
    slope,
    horizontal_distance_to_hydrology,
    hillshade_noon
  ) |>
  mutate(cover_type = as_factor(cover_type))

bi_forest_cover_split <- initial_split(binary_forest_tbl, prop = 0.7)
bi_forest_train <- training(bi_forest_cover_split)
bi_forest_test <- testing(bi_forest_cover_split)


cls_models <- logistic_reg() |>
  set_engine('glm')


binary_cls_receipe <- recipe(cover_type ~ ., data = bi_forest_train)

cls_workflow <-
  workflow() %>%
  add_model(cls_models) %>%
  add_recipe(binary_cls_receipe)


cls_fit <- cls_workflow |>
  fit(data = bi_forest_train)


res <- predict(cls_fit, new_data = bi_forest_test, type = "prob") |>
  bind_cols(predict(cls_fit, new_data = bi_forest_test)) |>
  bind_cols(bi_forest_test |> select(cover_type))


metrics(res, cover_type, .pred_class)

roc_value <- roc_auc(res, cover_type, .pred_1) |>
  select(.estimate) |>
  pull()


roc_curve(res, cover_type, .pred_1) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  annotate(
    "text",
    x = 0.5,
    y = 0.65,
    size = 8,
    label = str_glue('auc: {roc_value |> round(2) }')
  ) +
  coord_equal() +
  theme_minimal()


# 2 .0 Multinominal ----
#

multi_forest_cover_tbl <- forest_cover_tbl |>
  mutate(cover_type = as_factor(cover_type)) |>
  select(
    cover_type,
    elevation,
    aspect,
    slope,
    horizontal_distance_to_hydrology,
    hillshade_noon
  )

multi_forest_cover_split <- initial_split(multi_forest_cover_tbl, prop = 0.7)
multi_forest_train <- training(multi_forest_cover_split)
multi_forest_test <- testing(multi_forest_cover_split)


multinomal_cls_models <- multinom_reg(mode = "classification") %>%
  set_engine("nnet")


cls_receipe <- recipe(cover_type ~ ., data = multi_forest_train)

multinomnial_cls_workflow <- workflow() |>
  add_model(multinomal_cls_models) |>
  add_recipe(cls_receipe)

multinominal_fit <- multinomnial_cls_workflow |>
  fit(data = multi_forest_train)

multi_res <- predict(
  multinominal_fit,
  new_data = multi_forest_test,
  type = "prob"
) |>
  bind_cols(predict(multinominal_fit, new_data = multi_forest_test)) |>
  bind_cols(multi_forest_test |> select(cover_type))


cm <- multi_res |>
  conf_mat(cover_type, .pred_class)

autoplot(cm, type = "heatmap")


metrics(multi_res, cover_type, .pred_class)

# 3.0 Random Forest ----
rf_models <- rand_forest(mode = 'classification') |>
  set_engine('randomForest')


rf_cls_workflow <- workflow() |>
  add_model(rf_models) |>
  add_recipe(cls_receipe)

rf_fit <- rf_cls_workflow |>
  fit(data = multi_forest_train)

rf_res <- predict(
  rf_fit,
  new_data = multi_forest_test,
  type = "prob"
) |>
  bind_cols(predict(rf_fit, new_data = multi_forest_test)) |>
  bind_cols(multi_forest_test |> select(cover_type))


rf_res |>
  conf_mat(cover_type, .pred_class) |>
  autoplot(type = "heatmap")


metrics(rf_res, cover_type, .pred_class)


# 4.0 Random Forest - 1 ----
rf_1_models <- rand_forest(mode = 'classification') |>
  set_engine('ranger')


rf_1_cls_workflow <- workflow() |>
  add_model(rf_1_models) |>
  add_recipe(cls_receipe)

rf_1_fit <- rf_1_cls_workflow |>
  fit(data = multi_forest_train)

rf_1_res <- predict(
  rf_1_fit,
  new_data = multi_forest_test,
  type = "prob"
) |>
  bind_cols(predict(rf_1_fit, new_data = multi_forest_test)) |>
  bind_cols(multi_forest_test |> select(cover_type))


rf_1_res |>
  conf_mat(cover_type, .pred_class) |>
  autoplot(type = "heatmap")


metrics(rf_1_res, cover_type, .pred_class)
