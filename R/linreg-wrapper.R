#' Perform linear regression
#'
#' Use \code{linreg} to perform linear regressions.
#'
#' @param formula an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @param data a data frame or (or object coercible by
#'   \code{\link{as.data.frame}}).
#'
#' @return \code{linreg} returns an object of class "\code{Linreg}".
#'
#'   An object of class "\code{Linreg}" have the following methods:
#'   \item{coef()}{a named vector of coefficients}
#'   \item{resid()}{the residuals, that is response minus fitted values}
#'   \item{pred()}{predicted values of of a model}
#'
#' @seealso \code{\link{lm}}, \code{\link{class}}, \code{\link{formula}}
#'
#' @examples
#' linreg(formula = Sepal.Length ~ Sepal.Width, data = iris)
#'
#' \dontrun{
#' linreg(TRUE, TRUE)
#' }
#' @export

linreg <- function(formula, data) {
  suppressMessages( # without suppressing output of testthaat looks horrible
    devtools::use_package("Matrix")
  )
  # Error handling -------------------------------------------------------------
  # Check whether inputs can be coerced to formula and data.frame
  canCoerse <- TRUE
  tryCatch({
    formula <- as.formula(formula)
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
  regressors <- model.matrix(formula, data)
  regressorsRank <- Matrix::rankMatrix(regressors)
  if (regressorsRank < ncol(regressors)) {
    stop("multicollinearity of regressors")
  }

  # Body -----------------------------------------------------------------------
  linregObject <- Linreg$new(formula = formula,
                             data = data,
                             call = match.call())
  return(invisible(linregObject))
}
