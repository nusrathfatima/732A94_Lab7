#' Perform ridge regression
#'
#' Use \code{ridgereg} to perform ridge regressions.
#'
#' @param formula an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @param data a data frame or (or object coercible by
#'   \code{\link{as.data.frame}}).
#' @param lambda lambda of a model.
#'
#' @return \code{ridgereg} returns an object of class "\code{Ridgereg}".
#'
#'   An object of class "\code{Ridgereg}" have the following methods:
#'   \item{coef()}{a named vector of coefficients}
#'   \item{resid()}{the residuals, that is response minus fitted values}
#'   \item{pred()}{predicted values of of a model}
#'   \item{print()}{print the model}
#'   \item{plot()}{plot the residuals of the model}
#'
#' @seealso \code{\link{lm}}, \code{\link{class}}, \code{\link{formula}}
#'
#' @import methods
#' @examples
#' ridgereg(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = iris,lambda=0)
#'
#' \dontrun{
#' linreg(TRUE, TRUE)
#' }
#' @export

ridgereg <- function(formula, data, lambda) {
  # Error handling -------------------------------------------------------------
  # Check whether inputs can be coerced to formula and data.frame
  canCoerse <- TRUE
  tryCatch({
    formula <- stats::as.formula(formula)
    data <- as.data.frame(data)
  }, error = function(e) {
    canCoerse <<- FALSE
  })
  if(!canCoerse) {
    stop("wrong parameters")
  }

  # Check whether variables from formula are in data
  if(!all(all.vars(formula) %in% colnames(data))) {
    stop("variable(s) not in data")
  }

  # as.data.frame(data) should contain at leas 1 row/column
  if(!all(dim(data) > 0)) {
    stop("data must have >=1 cols/rows")
  }

  # check for multicollinearity of regressors
  regressors <- stats::model.matrix(formula, data)
  regressorsRank <- Matrix::rankMatrix(regressors)
  if (regressorsRank < ncol(regressors)) {
    stop("multicollinearity of regressors")
  }

  # Check if at least 2 regressors (+1 because of intercept)
  if (ncol(regressors) < 2 + 1) {
    stop("need at least 2 regressors")
  }


  # Body -----------------------------------------------------------------------
  ridgeregObject <- Ridgereg$new(formula = formula,
                                 data = data,
                                 lambda = lambda,
                                 call = match.call())
  return(invisible(ridgeregObject))
}
