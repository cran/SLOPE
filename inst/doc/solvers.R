## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(SLOPE)

response <- "binomial"

data <- SLOPE:::randomProblem(n = 100, p = 1000, response = response)

solvers <- c("hybrid", "fista", "pgd")

fits <- lapply(solvers, function(solver) {
  fit <- SLOPE(
    data$x,
    data$y,
    family = response,
    solver = solver,
    diagnostics = TRUE
  )
})

## ----compute-total-time-------------------------------------------------------
total_time <- sapply(
  fits,
  function(x) {
    sum(sapply(x$diagnostics[["time"]], tail, n = 1))
  }
)

names(total_time) <- solvers

barplot(total_time, ylab = "Time (s)")

## ----collect-gaps-------------------------------------------------------------
pen_minmax <- min(
  vapply(fits, function(fit) {
    max(fit$diagnostics$penalty)
  }, FUN.VALUE = numeric(1))
)

res <- lapply(seq_along(fits), function(i) {
  fit <- fits[[i]]
  solver <- solvers[[i]]
  d <- fit$diagnostics[fit$diagnostics$penalty == pen_minmax, ]
  data.frame(
    solver = solver,
    time = d$time,
    gap = d$primal - d$dual
  )
})

## ----plot-gaps, fig.width = 6, fig.height = 4.5-------------------------------
ylim <- range(unlist(lapply(res, function(x) x$gap)))

colors <- palette.colors(
  n = length(solvers),
  palette = "Okabe-Ito"
)

plot(
  res[[1]]$time,
  res[[1]]$gap,
  type = "n",
  ylim = ylim,
  xlab = "Time (s)",
  ylab = "Duality gap",
  log = "y"
)

for (i in seq_along(solvers)) {
  lines(res[[i]]$time, res[[i]]$gap, col = colors[i])
}

legend("topright", legend = solvers, col = colors, lty = 1)

