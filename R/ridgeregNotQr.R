.ridgereg <- function(formula, data, lambda) {
  "Computes and returns coefficients of the model"
  #
  # Args:
  #
  # Returns:
  #   Named vector of estimated coefficients.


  # Extract X matrix and Y matrix (vector) from data and formula
  X <- model.matrix(formula, data)
  #X[, -1] <- apply(X[, -1], MARGIN = 2,
  #FUN = function(x) (x-mean(x))/sd(x))
  yName <- all.vars(formula)[1]
  Y <- data[, yName]

  # Estimates
  bridgeHat <- solve(t(X) %*% X + diag(lambda, nrow = ncol(X))) %*% t(X) %*% Y

  # Format in the same way as lm()
  bridgeHat <- bridgeHat[, 1]

  # Store the result in cache
  return(bridgeHat)
}
