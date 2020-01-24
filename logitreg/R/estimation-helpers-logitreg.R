# ------------------------------------------------------------------------------
#' Assertion of inputs for logistic regression
#'
#' Assert that input data for logistic regression model follow necessary
#' conditions for estimation
#'
#' @param response     vector of responses
#' @param design       design matrix for the model
#' @param coefs        start values for coefficients
#'
#' @import checkmate
#' @return vector of coefficients if assertion successful; otherwise error
#' @md
check_inputs <- function(response, design, coefs) {

  # check response
  assert_vector(response, any.missing = FALSE, min.len = 1)
  assert_true(all.equal(unique(response), c(0, 1)))

  # check design matrix
  design <- try(as.matrix(design))
  assert_matrix(design, mode = "numeric", any.missing = FALSE,
                nrows = length(response), min.cols = 1, min.rows = 1)

  # check coefficients
  # use default start values if none given
  if (is.null(coefs)) coefs <- rep(1, ncol(design))
  assert_numeric(coefs, finite = TRUE, any.missing = FALSE,
                 len = ncol(design))
  coefs
}


#-------------------------------------------------------------------------------
#' Computation of negative log-likelihood for logistic regression
#'
#' Compute negative log-likelihood of data based on the logistic distribution
#'
#' @inheritParams check_inputs
#' @return negative log-likelihood for given data and coefficients
#' @md
neg_loglik <- function(response, design, coefs) {
  probabilities <- logistic(design %*% coefs)
  - sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}


#------------------------------------------------------------------------------
#' Computation of the negative gradient of the log-likelihood
#'
#' Compute the first negative derivative / gradient of the log-likelihood
#' for given data and coefficients based on the logistic distribution
#'
#' @inheritParams check_inputs
#' @return negative gradient values for given coefficients
#' @md
neg_loglik_deriv <- function(response, design, coefs) {
  probabilities <- logistic(design %*% coefs)
  - t(response - probabilities) %*% design
}


#-------------------------------------------------------------------------------
#' Computation of values of distribution function for the logistic distribution
#'
#' Compute distribution function of the logistic distribution for the given `x`
#'
#' @seealso [stats::Logistic]
#' @param x single numeric value
#' @importFrom stats plogis
#' @return distribution function
#' @md
logistic <- function(x) plogis(x)
