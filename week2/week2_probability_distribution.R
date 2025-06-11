dbinom(x = 5, size = 10, prob = 0.5) # Probability of 5 successes in 10 trials
pbinom(q = 5, size = 10, prob = 0.5) # Cumulative probability of 5 successes
qbinom(p = 0.7, size = 10, prob = 0.5) # Smallest number of successes with 70%
rbinom(n = 5, size = 10, prob = 0.5) # 5 random success counts from 10 trials

set.seed(342845) # Set random seed
rbinom(n = 10, size = 1, prob = 0.5) # Create dummy variable


rpois(n = 10, lambda = 3) # Random values from poisson distribution

rnorm(n = 15) # Random values from normal distribution

pnorm(q = 1.96, mean = 0, sd = 1) # Cumulative probability up to 1.96

library(tidyverse)

x <- seq(-4, 4, length = 100) # Generate points from normal distribution
y <- dnorm(x, mean = 0, sd = 1)
my_dnorm <- tibble(x, y)

ggplot(my_dnorm, aes(x = x, y = y)) + # Draw normal distribution
  geom_line()


my_rdata <- tibble(
  x = c(
    rbinom(n = 1000, size = 10, prob = 0.5), # Random data
    rpois(n = 1000, lambda = 3),
    rnorm(n = 1000, mean = 1, sd = 0.75),
    runif(n = 1000, min = 0, max = 5),
    rexp(n = 1000)
  ),
  dist = c(
    rep("rbinom", 1000),
    rep("rpois", 1000),
    rep("rnorm", 1000),
    rep("runif", 1000),
    rep("rexp", 1000)
  )
)
my_rdata <- my_rdata |>
  mutate(
    dist = factor(dist, levels = c("rbinom", "rpois", "rnorm", "runif", "rexp"))
  )


ggplot(my_rdata, aes(x = x)) + # Draw grid of distributions
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "#1b98e0",
    col = "#353436"
  ) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~dist, ncol = 1) +
  labs(
    title = "Histograms with Density Overlay of Different Random Distributions",
    x = "Value",
    y = "Density"
  ) +
  theme_bw()

## shapiro test provides test for normality. H0- data is normal distribution. H1- it is otherwise
## p-value <= 0.05 , data is not normal distributed
##
##

shapiro.test(my_rdata$x[my_rdata$dist == "rnorm"]) |> broom::tidy() # Shapiro-Wilk normality test

# # A tibble: 1 × 3
# statistic p.value method
# <dbl>   <dbl> <chr>
#     1     0.998   0.232 Shapiro-Wilk normality test

my_rdata |>
  group_by(dist) |>
  nest() |>
  mutate(shapiro = map(data, ~ broom::tidy(shapiro.test(.x$x)))) |>
  unnest(shapiro)

# dist   data                 shapiro$statistic $p.value
# <fct>  <list>                           <dbl>    <dbl>
# rbinom <tibble [1,000 × 1]>             0.966 1.62e-14
# rpois  <tibble [1,000 × 1]>             0.943 4.30e-19
# rnorm  <tibble [1,000 × 1]>             0.998 2.32e- 1
# runif  <tibble [1,000 × 1]>             0.953 1.92e-17
# rexp   <tibble [1,000 × 1]>             0.856 3.19e-29
