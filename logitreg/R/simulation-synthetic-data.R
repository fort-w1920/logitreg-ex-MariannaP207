#' Simulation of synthetic data
#'
#' Simulate synthetic data for tests
#'
#' @param n number of rows
#' @param numerics number of numeric covariates
#' @param factors number of factorisl covariates
#' @param seed seed for reproducibility
#' @param dataframe logical; if `TRUE` dataframe as output, otherwise matrix
#' @importFrom stats runif rbinom rnorm
#' @return list with three components:
#' * `design` model matrix for covariates
#' * `response` numeric vector of responses
#' * `coefs` true coefficients with which synthetic data are simulated
#' @md

sim_data <- function(n = 1500, numerics = 3, factors = 0, seed = NULL, dataframe = FALSE) {
  # set RNG seed for reproducibility:
  if (!is.null(seed)) {
    set.seed(seed)
  }

  covariates <- 1 + numerics + factors

  design <- matrix(0, nrow = n, ncol = covariates)
  design[, 1] <- 1
  design[, seq_len(numerics) + 1] <- matrix(rnorm(n * numerics), nrow = n)
  if (factors) {
    # add binary factors
    dummies <- matrix(sample(c(0, 1), n * factors, replace = TRUE), nrow = n)
    design[, -seq_len(1 + numerics)] <- dummies
  }

  coefs <- runif(covariates, min = -3, max = 3)

  probabilities <- logistic(design %*% coefs)
  response <- rbinom(n, prob = probabilities, size = 1)

  if (!dataframe) {
    return(list(design = design, response = response, coefs = coefs))
  }
  structure(
    data.frame(response = response, design[, -1, drop = FALSE]),
    coefs = coefs
  )
}
