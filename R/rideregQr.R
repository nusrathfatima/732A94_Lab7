#' Perform ridge regression using QR decomposition
#'
#' Use \code{.ridgeregQr} to perform ridge regression.
#'
#' @param formula an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @param data a data frame or (or object coercible by
#'   \code{\link{as.data.frame}}).
#' @param lambda the lambda of the data set. Default is 0.
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
#' .ridgeregQr(formula = Sepal.Length ~ Sepal.Width, data = iris, lambda=0)
#'
#' \dontrun{
#' .ridgeregQr(TRUE, TRUE)
#' }
#' @export
.ridgeregQr <- function(formula, data, subset, na.action, lambda = 0, model = FALSE, x = FALSE, y = FALSE, contrasts = NULL, ...) {
  m <- match.call(expand.dots = FALSE)
  m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  Y <- model.response(m)
  X <- model.matrix(Terms, m, contrasts)
  n <- nrow(X)
  p <- ncol(X)
  offset <- model.offset(m)
  if (!is.null(offset))
    Y <- Y - offset
  Inter<-0
   Ym <- Xm <- NA
  Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
  X <- X/rep(Xscale, rep(n, p))
  Xs <- svd(X)
  rhs <- t(Xs$u) %*% Y
  d <- Xs$d
  #this is the actual formula used to determine coefficients
  #save coefficients as lscoef
  answer.qr = qr(crossprod(X) + diag(n*lambda,p,p))
  lscoef = qr.coef(answer.qr,crossprod(X,Y))
  #only modify the two lines above
  lsfit <- X %*% lscoef
  resid <- Y - lsfit
  s2 <- sum(resid^2)/(n - p - Inter)
  HKB <- (p - 2) * s2/sum(lscoef^2)
  LW <- (p - 2) * s2 * n/sum(lsfit^2)
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(lambda, rep(dx, k))
  a <- drop(d * rhs)/div
  dim(a) <- c(dx, k)
  coef <- Xs$v %*% a
  dimnames(coef) <- list(names(Xscale), format(lambda))
  GCV <- colSums((Y - X %*% coef)^2)/(n - colSums(matrix(d^2/div,
                                                         dx)))^2
  res <- list(coef = drop(coef), scales = Xscale, Inter = Inter,
              lambda = lambda, ym = Ym, xm = Xm, GCV = GCV, kHKB = HKB,
              kLW = LW)
  #not sure how to do this
  #return(invisible(res))
  #set class here
  class(res) <- "ridgelm"
  res
}
