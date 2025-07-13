library(tidyverse)
library(boot)


set.seed(658445) # Set random seed

Price <- c(
  rnorm(100, mean = 22000, sd = 3000), # Create example data
  rnorm(100, mean = 30000, sd = 3000),
  rnorm(100, mean = 30100, sd = 3000)
)
Price_paired <- Price + rnorm(300, mean = 2000, sd = 500)
Mileage <- rnorm(300, mean = 50000, sd = 10000) - 1.2 * Price


data <- tibble(
  Price,
  Price_paired,
  Mileage,
  Group = factor(rep(1:3, each = 100))
)

wilcox.test(data$Price, data$Price_paired, paired = TRUE) |> broom::tidy()


wilcox.test(data$Price[data$Group == 1], data$Price[data$Group == 2]) |>
  broom::tidy()


kruskal.test(
  Price ~ Group, # Kruskal-Wallis Test
  data = data
) |>
  broom::tidy()

pairwise.wilcox.test(data$Price, data$Group) |> broom::tidy()


cor.test(
  data$Price, # Spearman's Rank Correlation
  data$Mileage,
  method = "spearman"
) |>
  broom::tidy()


ggplot(data, aes(x = Mileage, y = Price)) + # Kernel Smoothing Regression
  geom_point() +
  geom_smooth(method = "loess", span = 0.4, linewidth = 1.5) +
  labs(title = "Kernel Smoothing Regression for Price vs. Mileage") +
  theme_minimal()


mean_func <- function(data, indices) {
  # Function for Bootstrap mean
  return(mean(data$Price[indices]))
}

boot_result <- boot(
  data = data, # Perform Bootstrap
  statistic = mean_func,
  R = 1000
)

mean(data$Price) # Traditional mean() function

boot_result # Bootstrap results
boot_ci <- boot.ci(boot_result, type = "basic")
boot_ci


boot_vline_data <- data.frame(
  xintercept = c(
    mean(data$Price), # Data frame for plot
    mean(boot_result$t),
    boot_ci$basic[4],
    boot_ci$basic[5]
  ),
  label = c(
    "Original Mean",
    "Bootstrapped Mean",
    "95% CI Lower & Upper",
    "95% CI Lower & Upper"
  )
)


ggplot(
  data.frame(boot_result$t), # Histogram of bootstrap results
  aes(x = boot_result$t)
) +
  geom_histogram(binwidth = 50, fill = "#A5C3FF", col = "#3943B7") +
  geom_vline(
    data = boot_vline_data,
    aes(xintercept = xintercept, col = label),
    linetype = c("dashed", "dashed", "solid", "solid"),
    linewidth = c(1.5, 1.5, 1, 1)
  ) +
  scale_color_manual(values = c("#6B2737", "#E05046", "#86F979")) +
  labs(
    title = "Original & Bootstrapped Means & Confidence Intervals",
    x = "Bootstrapped Mean",
    y = "Frequency",
    col = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
