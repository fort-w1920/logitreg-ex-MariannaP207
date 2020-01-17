#' Fitting Logistic Regression Model
#'
#' $fit_logitreg$ is used to fit a logistic regression model minimizing
#' the negative log-likelihood of the given data.
#'
#' @param design    design matrix of independent variables
#' @param response  numeric vector of a dependent variable
#' @param coefs     numeric vector for initial values of coefficients with which
#'                  the log-likelihood must be optimized
#' @param ...       further arguments to be passed, see [optim]
#'
#' @importFrom      stats optim
#' @return          a list with components:
#' * $coefficients$   best coefficients found to fit given data
#' * $fitted$         estimated probabilities
#' * $data$           a list with initial $design$ and $response$ data for which
#'                    the log-likelihood is optimized
#' @export
#' @md
#'
fit_logitreg <- function(design, response, coefs = NULL, ...) {

  coefs <- check_inputs(design, response, coefs)

  # find coefficients <par> that optimize function <fn>
  model <- optim(par = coefs,
                 fn = neg_loglik,
                 gr = neg_loglik_deriv,
                 design = design,
                 response = response, ...)

  # save estimated coefficients
  coefficients <- model[["par"]]
  # fit logistic regression model to the data using the estimated coefficients
  fitted <- logistic(design %*% coefficients)
  # save initial data
  data <- list(design, response)

  list("coefficients" = coefficients,
       "fitted" = fitted,
       "data" = data)
}


# ------------------------------------------------------------------------------
#' Assertion of inputs for logistic regression
#'
#' Assert that input data for logistic regression model follow necessary
#' conditions for estimation
#'
#' @inheritParams fit_logitreg
#' @import checkmate
#' @return vector of coefficients if assertin successful; otherwise error
#' @md
check_inputs <- function(design, response, coefs) {

  # check response
  assert_numeric(response, any.missing = FALSE, min.len = 1, finite = TRUE)

  # check design matrix
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
#' @inheritParams fit_logitreg
#' @return negative log-likelihood for given data and coefficients
#' @md
neg_loglik <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  - sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}


#------------------------------------------------------------------------------
#' Computation of the negative gradient of the log-likelihood
#'
#' Compute the first negative derivative / gradient of the log-likelihood
#' for given data and coefficients based on the logistic distribution
#'
#' @inheritParams neg_loglik
#' @return negative gradient values for given coefficients
#' @md
neg_loglik_deriv <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  - t(response - probabilities) %*% design
}


#-------------------------------------------------------------------------------
#' Computation of values of distribution function for the logistic distribution
#'
#' Compute distribution function of the logistic distribution for the given $x$
#'
#' @seealso [stats::Logistic]
#' @param x single numeric value
#' @importFrom stats plogis
#' @return distribution function
#' @md
logistic <- function(x) plogis(x)
