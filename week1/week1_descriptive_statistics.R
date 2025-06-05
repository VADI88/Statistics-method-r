# import library
library(tidyverse)
library(skimr)
library(gtExtras)
library(svglite)
library(corrplot)

# load the data
#
iris_tbl <- iris |> as_tibble()

## Manually creating the summary values
iris_tbl |>
  summarise(
    mean_speal_length = mean(Sepal.Length),
    mean_speal_width = mean(Sepal.Width),
    median_speal_length = median(Sepal.Length),
    median_speal_width = median(Sepal.Width),
    var_speal_length = var(Sepal.Length),
    var_speal_width = var(Sepal.Width),
  )
## Base R packages.
iris_tbl |>
  summary()

## using skimr package

iris_tbl |>
  skim() |>
  to_long()


## Using GTExtras
gt_plt_summary(iris)


## Correlation  - Using only Numeric values

iris_numeric_tbl <- iris_tbl |>
  select_if(is.numeric)

iris_numeric_tbl |>
  cor()

### Ploting the corrplot to identify the correlation between variables.
corrplot(iris_numeric_tbl |> cor(), method = 'square', addCoef.col = "red")


#### Summarize statistics by grouping
####

iris_aggr_tbl <- iris_tbl |>
  group_by(Species) |>
  summarize(across(where(is.numeric), mean))


ggplot(iris_aggr_tbl) +
  aes(x = Species, y = Sepal.Length, fill = Species) +
  geom_col()


iris_aggr_long_tbl <- iris_tbl |>
  pivot_longer(
    cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  ) |>
  group_by(Species, name) |>
  summarize(avg_value = mean(value)) |>
  ungroup()


ggplot(iris_aggr_long_tbl) +
  aes(x = Species, y = avg_value, fill = name) +
  geom_col(position = "dodge") +
  labs(title = "Mean by Species", x = "Species", y = "Mean") +
  guides(fill = guide_legend(title = NULL))
