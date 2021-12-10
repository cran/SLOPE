## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----packages, message = FALSE------------------------------------------------
library(SLOPE)
library(tidyr)
library(dplyr)
library(bench)

## ----benchmark----------------------------------------------------------------
res <- expand_grid(
  p = seq(10, 10000, length.out = 10),
  i = 1:20,
  method = c("stack", "pava"),
  time = NA
)

set.seed(2254)

for (i in seq_len(nrow(res))) {
  p <- res$p[i]

  x <- rnorm(p)
  lambda <- sort(runif(p), decreasing = TRUE)

  time <- bench_time(sortedL1Prox(x, lambda, res$method[i]))

  res$time[i] <- time[["real"]] * 1e3 # milliseconds
}

## ---- fig.cap = "Comparison in execution times between the PAVA algorithm and the stack-based algorithm for solving the SLOPE prox.", fig.width = 5----
library(ggplot2)
library(scales)

ggplot(res, aes(p, time, fill = method, col = method)) +
  stat_summary(
    geom = "ribbon",
    fun.data = mean_se,
    alpha = 0.2,
    col = "transparent"
  ) +
  stat_summary(geom = "line", fun = mean) +
  scale_y_log10() +
  labs(y = "Time (milliseconds)", x = expression(p))

