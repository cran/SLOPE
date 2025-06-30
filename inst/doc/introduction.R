## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(SLOPE)

x <- heart$x
y <- heart$y

fit <- SLOPE(x, y, family = "binomial", lambda = "bh")

## ----fig.cap = "Regularization path for a binomial regression model fit to the heart data set.", fig.width = 6, fig.height = 5----
plot(fit)

## -----------------------------------------------------------------------------
set.seed(924)

x <- bodyfat$x
y <- bodyfat$y

tune <- trainSLOPE(
  x,
  y,
  q = c(0.1, 0.2),
  number = 5,
  repeats = 2
)

## ----fig.cap = "Model tuning results from Gaussian SLOPE on the bodyfat dataset.", fig.width = 5.5, fig.height = 3----
plot(tune, measure = "mae") # plot mean absolute error

## -----------------------------------------------------------------------------
tune

## ----fig.cap = "Control of false discovery rate using SLOPE.", fig.width = 4----
# proportion of real signals
q <- seq(0.05, 0.5, length.out = 20)
fdr <- double(length(q))
set.seed(1)

for (i in seq_along(q)) {
  n <- 1000
  p <- n / 2
  alpha <- 1
  problem <- SLOPE:::randomProblem(n, p, q = q[i], alpha = alpha)

  x <- problem$x
  y <- problem$y
  signals <- problem$nonzero

  fit <- SLOPE(x,
    y,
    lambda = "gaussian",
    q = 0.1,
    alpha = alpha / sqrt(n)
  )

  selected_slope <- which(as.matrix(fit$nonzeros[[1]]))
  v <- length(setdiff(selected_slope, signals))
  r <- length(selected_slope)
  fdr[i] <- v / max(r, 1)
}

# Create the plot
plot(
  q,
  fdr,
  type = "o",
  xlab = "q",
  ylab = "FDR",
  pch = 16,
  las = 1
)

# Add horizontal line at 0.1
abline(h = 0.1, lty = 3)

