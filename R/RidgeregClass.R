#' A Reference Class to a ridgereg regression model.
#'
#' @field formula an object of \code{\link{class}} \code{\link{formula}}: a
#'   symbolic description of the model to be fitted.
#' @field data a data frame
#' @field lambda lambda of a model
#' @field cache a list that contains cached values for results of methods
#'   \code{coef()}, \code{resid()}, \code{pred()} and the hash values of fields
#'   \code{formula} and \code{data} used to compute the output
#'

# How caching works
#
# 1. Before computing anything, check if the result is already stored in cache.
# 2a. If the result is stored, then return it.
# 2b. If the result is not stored, then compute it, save it in cache, return it
#
# Structure of the cache list:
# cache$
#       coef[["hash", "value"]]
#       resid[["hash", "value"]]
#       pred[["hash", "value"]]


# Class ------------------------------------------------------------------------
Ridgereg <- setRefClass(
  "Ridgereg",
  contains = "Linreg",
  # Fields ---------------------------------------------------------------------
  fields = list(formula = "formula",
                data = "data.frame",
                lambda = "numeric",
                cache = "list",
                call = "language"),
  methods = list(
    # Methods --------------------------------------------------------------------
    coef = function() {
      "Computes and returns coefficients of the model"
      #
      # Args:
      #
      # Returns:
      #   Named vector of estimated coefficients.

      # Check if the result is already cached
      if (isCached("coef")) {
        return(.self$cache$coef$value)
      }

      # Calls external function .ridgeregQr
      ridgeregResult <- .ridgeregQr(formula = .self$formula,
                                    data = .self$data,
                                    lambda= .self$lambda)

      # Format in the same way as lm()
      X <- model.matrix(.self$formula, .self$data)
      X<- X[,-(1)]
      betaHat.unscaled <- ridgeregResult$coef
      betaHat.scaled <- betaHat.unscaled/ridgeregResult$scales
      interceptHat<-ridgeregResult$ym -  mean(X %*% betaHat.scaled)
      betaHat<-append(betaHat.scaled,interceptHat,after=0)
      # Store the result in cache
      storeCache("coef", betaHat)
      return(betaHat)
    },
    predict = function() {
      "Computes and returns predicted values of the model"
      #
      # Args:
      #
      # Returns:
      #   Named vector of predicted values.

      # Check if the result is cached
      if (isCached("predict")) {
        return(.self$cache$predict$value)
      }

      # Extract X matrix and Y matrix (vector) from data and formula
      X <- model.matrix(.self$formula, .self$data)

      # Get estimated coefficients
      betaHat <- .self$coef()

      yHat <- betaHat %*% t(X)
      # # Format in the same way as lm()
      yHatVector <- as.vector(yHat)
      names(yHatVector) <- colnames(yHat)
      yHat <- yHatVector

      # Store result in cache
      storeCache("predict", yHat)

      return(yHat)
    },
    isCached = function(methodName) {
      "Checks whether the result of a method is stored in cache"
      #
      # Args:
      #   method: A character string with the name of the method to check for
      #           whether its result is alread cached
      #
      # Returns:
      #   TRUE if the result is cached. FALSE if it is not.

      # Check if it is NULL (never initialized)
      if (is.null(.self$cache[[methodName]]$hash)) {
        return(FALSE)
      }

      # Check if hash of current data, formula is the same as it was when cache
      # was computed
      currentHash <- digest::digest(list(.self$formula,
                                         .self$data,
                                         .self$lambda), algo = "md5")
      if (currentHash == .self$cache[[methodName]]$hash) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    storeCache = function(methodName, value) {
      "Stores result for the method, computes hash of data and stores it"
      # used to compute the value
      #
      # Args:
      #   method: character name of the method
      #   value:  value that the method returns
      #
      # Returns:
      #   Nothing, but modifies fields of a Linreg object


      # Calculate hash of the list with two objects
      # - formula
      # - data
      currentHash <- digest::digest(list(.self$formula,
                                         .self$data,
                                         .self$lambda), algo = "md5")
      # Store hash in the cache list under the appropriate method
      .self$cache[[methodName]] <- list(
        hash = currentHash,
        value = value
      )

      return(invisible(currentHash))
    }
  )
)
