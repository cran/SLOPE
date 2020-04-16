#' Train a SLOPE model
#'
#' This function trains a model fit by [SLOPE()] by tuning its parameters
#' through cross-validation.
#'
#' Note that by default this method matches all of the available metrics
#' for the given model family against those provided in the argument
#' `measure`. Collecting these measures is not particularly demanding
#' computationally so it is almost always best to leave this argument
#' as it is and then choose which argument to focus on in the call
#' to [plot.TrainedSLOPE()].
#'
#' @section Parallel operation:
#' This function uses the **foreach** package to enable parallel
#' operation. To enable this, simply register a parallel backend
#' using, for instance, `doParallel::registerDoParallel()` from the
#' **doParallel** package before running this function.
#'
#' @inheritParams SLOPE
#' @param number number of folds (cross-validation)
#' @param repeats number of repeats for each fold (for repeated *k*-fold cross
#'   validation)
#' @param measure measure to try to optimize; note that you may
#'   supply *multiple* values here and that, by default,
#'   all the possible measures for the given model will be used.
#' @param ... other arguments to pass on to [SLOPE()]
#'
#' @return An object of class `"TrainedSLOPE"`, with the following slots:
#' \item{summary}{a summary of the results with means, standard errors,
#'                and 0.95 confidence levels}
#' \item{data}{the raw data from the model training}
#' \item{optima}{a `data.frame` of the best (mean) values for the different metrics and their corresponding parameter values}
#' \item{measure}{a `data.frame` listing the used metrics and their labels}
#' \item{model}{the model fit to the entire data set}
#' \item{call}{the call}
#'
#' @export
#'
#' @seealso [foreach::foreach()], [plot.TrainedSLOPE()]
#'
#' @examples
#' # 8-fold cross-validation repeated 5 times
#' tune <- trainSLOPE(subset(mtcars, select = c("mpg", "drat", "wt")),
#'                    mtcars$hp,
#'                    q = c(0.1, 0.2),
#'                    number = 8,
#'                    repeats = 5)
trainSLOPE <- function(x,
                       y,
                       q = 0.2,
                       number = 10,
                       repeats = 1,
                       measure = c("mse",
                                   "mae",
                                   "deviance",
                                   "missclass",
                                   "auc"),
                       ...) {
  ocall <- match.call()

  n <- NROW(x)

  measure <- match.arg(measure, several.ok = TRUE)

  y <- as.matrix(y)

  stopifnot(NROW(x) > number,
            number > 1,
            repeats >= 1)

  # get initial penalty sequence
  fit <- SLOPE(x, y, ...)

  # match measure against accepted measure for the given family
  family <- fit$family

  ok <- switch(family,
               gaussian = c("mse", "mae"),
               binomial = c("mse", "mae", "deviance", "misclass", "auc"),
               poisson = c("mse", "mae"),
               multinomial = c("mse", "mae", "deviance"))
  measure <- measure[measure %in% ok]

  if (length(measure) == 0)
    stop("measure needs to be one of ", ok)

  sigma <- fit$sigma

  n_sigma <- length(sigma)
  n_q <- length(q)
  n_measure <- length(measure)

  fold_size <- ceiling(n/number)

  fold_id <- replicate(repeats, {
    matrix(c(sample(n), rep(0, number*fold_size - n)), fold_size, byrow = TRUE)
  })

  grid <- expand.grid(q = q,
                      fold = seq_len(number),
                      repetition = seq_len(repeats))

  # prevent warnings if no backend registered
  if (!foreach::getDoParRegistered())
    foreach::registerDoSEQ()

  i <- 1 # fixes R CMD check NOTE

  r <- foreach(i = seq_len(nrow(grid)), .packages = c("SLOPE")) %dopar% {
    id <- grid$fold[i]
    repetition <- grid$repetition[i]
    q <- grid$q[i]

    test_ind <- fold_id[, id, repetition]

    x_train <- x[-test_ind, , drop = FALSE]
    y_train <- y[-test_ind, , drop = FALSE]
    x_test  <- x[test_ind, , drop = FALSE]
    y_test  <- y[test_ind, , drop = FALSE]

    args <- utils::modifyList(list(x = x_train,
                                   y = y_train,
                                   q = q,
                                   sigma = sigma), list(...))
    s <- lapply(measure, function(m) {
      SLOPE::score(do.call(SLOPE::SLOPE, args), x_test, y_test, m)
    })

    unlist(s)
  }

  tmp <- array(unlist(r), c(n_sigma*n_q, n_measure, number*repeats))
  d <- matrix(tmp, c(n_sigma*n_q*n_measure, number*repeats))

  means <- rowMeans(d)
  se <- apply(d, 1, stats::sd)/sqrt(repeats*number)
  ci <- stats::qt(0.975, number*repeats - 1)*se
  lo <- means - ci
  hi <- means + ci

  summary <- data.frame(q = rep(q, each = n_sigma*n_measure),
                        sigma = rep(sigma, n_measure*n_q),
                        measure = rep(measure, each = n_sigma, times = n_q),
                        mean = means,
                        se = se,
                        lo = lo,
                        hi = hi)

  optima <- do.call(
    rbind,
    by(summary, as.factor(summary$measure), function(x) x[which.min(x$mean), ])
  )

  labels <- vapply(measure, function(m) {
    switch(
      m,
      deviance = {
        if (inherits(fit, "GaussianSLOPE"))
          "Mean-Squared Error"
        else if (inherits(fit, "BinomialSLOPE"))
          "Binomial Deviance"
        else if (inherits(fit, "PoissonSLOPE"))
          "Mean-Squared Error"
        else if (inherits(fit, "MultinomialSLOPE"))
          "Multinomial Deviance"
      },
      mse = "Mean Squared Error",
      mae = "Mean Absolute Error",
      accuracy = "Accuracy",
      auc = "AUC"
    )
  }, FUN.VALUE = character(1))

  rownames(summary) <- NULL
  rownames(optima) <- NULL

  structure(list(summary = summary,
                 data = d,
                 optima = optima,
                 measure = data.frame(measure = measure,
                                      label = labels,
                                      row.names = NULL,
                                      stringsAsFactors = FALSE),
                 model = fit,
                 call = ocall),
            class = "TrainedSLOPE")
}
