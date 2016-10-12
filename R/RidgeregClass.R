<<<<<<< HEAD
#' A Reference Class to a ridge regression model.
=======
#' A Reference Class to a ridgereg regression model.
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
#'
#' @field formula an object of \code{\link{class}} \code{\link{formula}}: a
#'   symbolic description of the model to be fitted.
#' @field data a data frame
<<<<<<< HEAD
#' @field cache a list that contains cached values for results of methods
#'   \code{coef()}, \code{pred()} and the hash values of fields
#'   \code{formula} and \code{data} used to compute the output
#'@export
=======
#' @field lambda lambda of a model
#' @field cache a list that contains cached values for results of methods
#'   \code{coef()}, \code{resid()}, \code{pred()} and the hash values of fields
#'   \code{formula} and \code{data} used to compute the output
#'
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807

# How caching works
#
# 1. Before computing anything, check if the result is already stored in cache.
# 2a. If the result is stored, then return it.
# 2b. If the result is not stored, then compute it, save it in cache, return it
#
# Structure of the cache list:
# cache$
#       coef[["hash", "value"]]
<<<<<<< HEAD
=======
#       resid[["hash", "value"]]
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
#       pred[["hash", "value"]]


# Class ------------------------------------------------------------------------
Ridgereg <- setRefClass(
  "Ridgereg",
<<<<<<< HEAD
=======
  contains = "Linreg",
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
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

<<<<<<< HEAD

      # Extract X matrix and Y matrix (vector) from data and formula
      X <- model.matrix(.self$formula, .self$data)
      #X[, -1] <- apply(X[, -1], MARGIN = 2,
                             #FUN = function(x) (x-mean(x))/sd(x))
      yName <- all.vars(.self$formula)[1]
      Y <- .self$data[, yName]

      # Estimates
      bridgeHat <- solve(t(X) %*% X + diag(lambda, nrow = ncol(X))) %*% t(X) %*% Y

      # Format in the same way as lm()
      bridgeHat <- bridgeHat[, 1]

      # Store the result in cache
      storeCache("coef", bridgeHat)
      return(bridgeHat)
    },
    predict = function() {
      "Computes and returns predicted values of the model"
      #
      # Args:
      #
      # Returns:
      #   Named vector of predicted values.

      # Check if the result is cached
      if (isCached("prediction")) {
        return(.self$cache$prediction$value)
      }

      # Extract X matrix and Y matrix (vector) from data and formula
      X <- model.matrix(.self$formula, .self$data)
      #X[, -1] <- apply(X[, -1], MARGIN = 2,
                        #FUN = function(x) (x-mean(x))/sd(x))
      # Get estimated coefficients
      bridgeHat <- .self$coef()

      yHat <- bridgeHat %*% t(X)
      # # Format in the same way as lm()
      yHatVector <- as.vector(yHat)
      names(yHatVector) <- colnames(yHat)
      yHat <- yHatVector

      # Store result in cache
      storeCache("prediction", yHat)

      return(yHat)
    },
    print = function(digits = 3, ...){
      "Prints a very brief summary of a Linreg object."
      # This method is extensively based on print.lm()
      #
      # Args:
      #   digits: the minimum number of significant digits to be printed in
      #           values.
      #   ...:    further arguments
      #
      # Returns
      #   Prints a very brief summary of an object and invisibly returns it.

      # Heading
      cat("\nCall:\n", paste(deparse(.self$call), sep = "\n", collapse = "\n"),
          "\n\n", sep = "")

      # Coefficients
      cat("Coefficients:\n")
      #print(format(.self$coef(), digits = digits), print = 2L, quote = FALSE)
      print.default(format(.self$coef(), digits = digits), print.gap = 2L,
                    quote = FALSE)
      cat("\n")

      return(invisible(.self$copy))
    },
    summary = function(digits = 3, ...){
      "Prints a summary of a Ridgereg object."
      # This method is loosely based on summary.lm.ridge()
      #
      # Args:
      #   digits: the minimum number of significant digits to be printed in
      #           values.
      #   ...:    further arguments
      #
      # Returns
      #   Prints a summary of an object and invisibly returns it.
      n <- nrow(.self$data)
      df <- n - (length(.self$coef()))

      # Heading
      cat("\nCall:\n", paste(deparse(.self$call), sep = "\n", collapse = "\n"),
          "\n\n", sep = "")

      # Coefficients
      coef <- data.frame(Estimate = .self$coef())

      # Print coefficients table
      cat("Coefficients:\n")
      print.data.frame(coef, digits = digits)
      cat("---\n")
      cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
      cat("\n\n")


      return(invisible(.self))
=======
      # Check if the result is already cached
      if (isCached("coef")) {
        return(.self$cache$coef$value)
      }

      # Calls external function .ridgeregQr
      ridgeregResult <- .ridgeregQr(formula = .self$formula,
                                    data = .self$data,
                                    lambda= .self$lambda)

      # Format in the same way as lm()
      betaHat <- ridgeregResult$coef

      # Store the result in cache
      storeCache("coef", betaHat)
      return(betaHat)
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
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

<<<<<<< HEAD
      suppressMessages( # without suppressing output of testthaat looks horrible
        devtools::use_package("digest")
      )

=======
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
      # Check if it is NULL (never initialized)
      if (is.null(.self$cache[[methodName]]$hash)) {
        return(FALSE)
      }

      # Check if hash of current data, formula is the same as it was when cache
      # was computed
<<<<<<< HEAD
      currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
=======
      currentHash <- digest::digest(list(.self$formula,
                                         .self$data,
                                         .self$lambda), algo = "md5")
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
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

<<<<<<< HEAD
      suppressMessages( # without suppressing output of testthaat looks horrible
        devtools::use_package("digest")
      )
=======
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807

      # Calculate hash of the list with two objects
      # - formula
      # - data
<<<<<<< HEAD
      currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
=======
      currentHash <- digest::digest(list(.self$formula,
                                         .self$data,
                                         .self$lambda), algo = "md5")
>>>>>>> ca9124b600e1deff0be5eb5654b44da44c7e7807
      # Store hash in the cache list under the appropriate method
      .self$cache[[methodName]] <- list(
        hash = currentHash,
        value = value
      )

      return(invisible(currentHash))
    }
  )
)
