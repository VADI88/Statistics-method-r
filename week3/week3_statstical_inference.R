set.seed(6186595)
my_pop <- rnorm(
  n = 1000000, # Simulate a population
  mean = 50,
  sd = 10
)

mean_pop <- mean(my_pop) # Calculate population mean
sd_pop <- sd(my_pop) # Calculate population sd

sd_pop

my_samp <- sample(
  x = my_pop, # Draw sample from population
  size = 100
)
mean_samp <- mean(my_samp) # Estimate population mean
mean_samp

var_samp <- var(my_samp) # Estimate population variance
var_samp

sd_samp <- sd(my_samp) # Estimate population sd
sd_samp

se_samp <- sd_samp / sqrt(length(my_samp)) # Standard error of mean
se_samp

error_margin <- qt(0.975, df = length(my_samp) - 1) * se_samp # 95% confidence interval
error_margin

lower_bound <- mean_samp - error_margin # Lower bound
lower_bound

upper_bound <- mean_samp + error_margin # Upper bound
upper_bound


my_ttest <- t.test(
  x = my_samp, # Perform one-sample t-test
  mu = mean_pop
)
my_ttest |> broom::tidy() # Print t-test results

my_ttest2 <- t.test(
  x = my_samp, # One-sample t-test
  mu = 55
)
my_ttest2 |> broom::tidy()


## Show the distribution
##
# install.packages("ggplot2")                             # Install & load ggplot2
library(ggplot2)

ggplot(
  data.frame(my_samp), # Draw ggplot2 with results
  aes(x = my_samp)
) +
  geom_histogram(
    binwidth = 2, # Draw histogram
    fill = "lightblue",
    color = "#353436"
  ) +
  geom_vline(
    xintercept = mean_samp, # Draw vertical mean line
    color = "red",
    linetype = "dashed",
    linewidth = 1.5
  ) +
  geom_errorbarh(
    aes(
      xmin = lower_bound, # Draw error bar
      xmax = upper_bound,
      y = 12
    ),
    color = "#1b98e0",
    linewidth = 2
  ) +
  labs(
    title = "Sample Distribution with Confidence Interval",
    x = "Sample Data",
    y = "Frequency"
  ) +
  theme_minimal() +
  annotate(
    "text", # Annotate results text
    x = 70,
    y = 12,
    label = paste(
      "Point Estimate (Mean):",
      round(mean_samp, 2),
      "\nVariance Estimate:",
      round(var_samp, 2),
      "\nStandard Error:",
      round(se_samp, 2),
      "\n95% CI:",
      round(lower_bound, 2),
      "to",
      round(upper_bound, 2),
      "\np-Value for t-test:",
      round(my_ttest$p.value, 2)
    ),
    hjust = 0,
    vjust = 1,
    size = 4,
    color = "#353436"
  )
