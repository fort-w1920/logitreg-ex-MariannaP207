#' `logitreg` generic & methods
#'
#' The function is used to fit a logistic regression model minimizing
#' the negative log-likelihood of the given data.
#'
#' @param object an object of class "[formula]", see Details.
#' @param data   a data frame containing the variables in the model as described
#'               in the formula, see Details.
#' @param coefs  numeric vector for initial values of coefficients with which
#'               the log-likelihood must be optimized.
#' @param ...    further arguments to be used for optimization; see [optim].
#'
#' @details
#'  Alternatively, one can supply a vector of responses as an
#'  `object` and a design matrix (see [model.matrix]) as `data`. In this case
#'  the function will return a list of estimated coefficients, fitted values
#'  and initial data used for estimation. This is not advised as the returned
#'  object will be a simple list and not an object of class `logitreg`.
#'
#' @return an object of class `logitreg` with the following components:
#' * `coefficients` best coefficients found to fit given data
#' * `fitted` predictions with the best coefficients for the given data
#' * `data` a list with initial `design` matrix and
#'   `response` for which the log-likelihood is optimized
#' * `formula` initial `formula` used to fit the model
#' @export
#' @md

logitreg <- function(object, data, coefs = NULL, ...) {
  UseMethod("logitreg")
}




#' @describeIn logitreg used if a `formula` and a `data.frame` are supplied
#' @importFrom checkmate assert_data_frame
#' @importFrom stats model.matrix
#' @export
#' @md
logitreg.formula <- function(object, data, coefs = NULL, ...) {

  # identify response variable from formula
  formula <- object
  response <- all.vars(formula)[1]

  # for "formula" method data must be a data frame
  assert_data_frame(data, types = c("numeric", "logical", "factor"),
                    any.missing = FALSE, min.rows = 1, min.cols = 2)

  # call logitreg.default with data as model.matrix and object as response
  object <- data[, response]
  data <- model.matrix(formula, data)
  fit <- logitreg.default(object, data, coefs, ...)

  # return new object of class "logitreg"
  fit$formula <- formula
  new_logitreg(fit)

}




#' @describeIn logitreg used if a response vector and a design matrix are supplied
#' @importFrom stats optim
#' @export
#' @md
logitreg.default <- function(object, data, coefs = NULL, ...) {

  # for method "default" object is a vector of response variable
  # and data is a model.matrix
  response <- as.vector(object)
  design <- as.matrix(data)
  coefs <- check_inputs(response, design, coefs)

  # find coefficients <par> that optimize function <fn>
  model <- optim(par = coefs,
                 fn = neg_loglik,
                 gr = neg_loglik_deriv,
                 design = design,
                 response = response, ...)

  # save estimated coefficients
  coefficients <- model[["par"]]
  names(coefficients) <- colnames(design)

  # fit logistic regression model to the data using the estimated coefficients
  fitted <- logistic(design %*% coefficients)
  fitted <- ifelse(fitted >= 0.5, 1, 0)

  # save initial data
  data <- list(design = design, response = response)

  fit <- list(
       "coefficients" = coefficients,
       "fitted" = fitted,
       "data" = data)

  fit
}



#' Constructor for class `logitreg`
#'
#' Create an object of class `logitreg`
#'
#' @param fit model for which class `logitreg` must be set
#' @return an object of class [logitreg]
#' @md
new_logitreg <- function(fit) {
  structure(fit, class = "logitreg")
}
