#' method `predict` for class `logitreg`
#'
#' Predicts values for new data based on the [logitreg] object.
#'
#' @param object     object of class `logitreg`.
#' @param newdata    optional data frame with new data for which predictions
#'                   should be calculated. If omitted, fitted values are returned.
#' @param ...        further arguments.
#'
#' @return a vector of predictions.
#' @importFrom       checkmate assert_data_frame assert_true
#' @importFrom       stats predict
#' @export
#' @md
predict.logitreg <- function(object, newdata, ...) {

  if (missing(newdata)) return(as.vector(object[["fitted"]]))

  assert_data_frame(newdata, any.missing = FALSE, min.rows = 1, min.cols = 2)
  coefs <- object[["coefficients"]]
  design <- model.matrix(object$formula, newdata)
  assert_true(length(coefs) == ncol(design))

  probs <- logistic(design %*% coefs)
  predictions <- ifelse(probs >= 0.5, 1, 0)
  as.vector(predictions)

}



#' method `fitted` for class `logitreg`
#'
#' Returns fitted values from the [logitreg] object.
#'
#' @param object    object of class `logitreg`.
#' @param ...       further arguments.
#'
#' @return a vector of fitted values
#' @importFrom stats fitted
#' @export
#' @md
fitted.logitreg <- function(object, ...) {
  fitted <- object[["fitted"]]
  as.vector(fitted)
}



#' method `coef` for class `logitreg`
#'
#' Returns estimated coefficients from the [logitreg] object.
#'
#' @inheritParams fitted.logitreg
#'
#' @return a vector of estimated coefficients
#' @importFrom      stats coef
#' @export
#' @md
coef.logitreg <- function(object, ...) {
  coefs <- object[["coefficients"]]
  coefs
}



#' method `summary` for class `logitreg`
#'
#' Produces summary of results for the [logitreg] object.
#'
#' @inheritParams fitted.logitreg
#'
#' @return a list of summary statistics:
#' * `Call` formula of the model
#' * `Coefficients` vector of estimated coefficients
#' * `Confusion` confusion table for true and fitted values
#' @export
#' @md
summary.logitreg <- function(object, ...) {
  summary_object <- list()
  summary_object[["Call"]] <- object[["formula"]]
  summary_object[["Coefficients"]] <- object[["coefficients"]]

  # create confusion table
  `true values` <- object[["data"]][["response"]]
  fitted <- object[["fitted"]]
  conf_table <- table(`true values`, fitted,
                      deparse.level = 2)

  summary_object[["Confusion"]] <- conf_table
  summary_object
}


#' method `plot` for class `logitreg`
#'
#' Plots a [ROC-curve](https://en.wikipedia.org/wiki/Receiver_operating_characteristic)
#' for the [logitreg] object.
#'
#' @param x         object of class `logitreg`.
#' @param y         optional if `x` has an appropriate structure.
#' @param ...       further parameters, such as graphical parameters (see [par]).
#'
#' @importFrom      pROC roc ggroc
#' @importFrom      graphics plot
#' @return a plot with a ROC-curve for the `logitreg` model
#' @export
#' @md
plot.logitreg <- function(x, y, ...) {

  object <- x
  true <- object[["data"]][["response"]]
  fitted <- object[["fitted"]]
  roc_o <- roc(true, predictor = as.vector(fitted))
  ggroc(roc_o)
}


