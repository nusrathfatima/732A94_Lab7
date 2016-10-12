#' A Reference Class to a linear regression model.
#'
#' @field formula an object of \code{\link{class}} \code{\link{formula}}: a
#'   symbolic description of the model to be fitted.
#' @field data a data frame
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
Linreg <- setRefClass(
  "Linreg",
  # Fields ---------------------------------------------------------------------
  fields = list(formula = "formula",
           data = "data.frame",
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

      # Extract X matrix and Y matrix (vector) from data and formula
      X <- model.matrix(.self$formula, .self$data)
      yName <- all.vars(.self$formula)[1]
      Y <- .self$data[, yName]

      # Estimates
      betaHat <- solve(t(X) %*% X) %*% t(X) %*% Y

      # Format in the same way as lm()
      betaHat <- betaHat[, 1]

      # Store the result in cache
      storeCache("coef", betaHat)
      return(betaHat)
    },
    resid = function() {
      "Computes and returns residuals of the model"
      #
      # Args:
      #
      # Returns:
      #   Named vector of residuals.

      # Check if the result is already cached
      if (isCached("resid")) {
        return(.self$cache$resid$value)
      }

      # Extract X matrix and Y matrix (vector) from data and formula
      X <- model.matrix(.self$formula, .self$data)
      yName <- all.vars(.self$formula)[1]
      Y <- .self$data[yName]

      # Get predicted values
      yHat <- .self$pred()

      epsilon <- Y - yHat

      # Format in the same way as lm()
      epsilonVector <- epsilon[, 1]
      names(epsilonVector) <- rownames(epsilon)
      epsilon <- epsilonVector

      # Store result in cache
      storeCache("resid", epsilon)

      return(epsilon)
    },
    pred = function() {
      "Computes and returns predicted values of the model"
      #
      # Args:
      #
      # Returns:
      #   Named vector of predicted values.

      # Check if the result is cached
      if (isCached("pred")) {
        return(.self$cache$pred$value)
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
      storeCache("pred", yHat)

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
    plot = function(){
      "Plots Residuals vs. Fits and Scale-Location graphs."
      require(ggplot2)
      X<-data.frame(pred=.self$pred(),resid=.self$resid())
      p1<-ggplot(X, aes(pred, resid))+
        geom_point()+
        geom_smooth(method="lm", na.rm=TRUE, color="red")+
        xlab(paste("Fitted Values\n",deparse(.self$call)))+
        ylab("Residuals")+
        ggtitle("Residual vs Fitted Plot")+
        theme_bw()
      
      p2<-ggplot(X, aes(pred, sqrt(abs(scale(resid)))))+
        geom_point(na.rm=TRUE)+
        geom_smooth(method="lm", na.rm = TRUE, color="red")+
        xlab(paste("Fitted Values\n",deparse(.self$call)))+
        ylab(expression(sqrt("|Standardized residuals|")))+
        ggtitle("Scale-Location")+
        theme_bw()
      
      return(list(rvfPlot=p1, sclLocPlot=p2))
    },
    summary = function(digits = 3, ...){
      "Prints a summary of a Linreg object."
      # This method is loosely based on summary.lm()
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

      # Standard errors
      sigmaHat <- 1 / df * sum(.self$resid()^2)
      X <- model.matrix(.self$formula, .self$data)
      varCovar <- sigmaHat * solve(t(X) %*% X)
      se <- sqrt(diag(varCovar))
      coef[["Std. Error"]] <- se

      # t-values
      tstat <- coef[["Estimate"]] / se
      coef[["t value"]] <- tstat

      # p-value
      pval <- 2 * pt(abs(coef[["t value"]]), df, lower.tail = FALSE)
      coef[["Pr(>|t|)"]] <- pval

      # asterisks
      asterisks <- ifelse(pval < 0.0001, "***",
                          ifelse(pval < 0.001, "**",
                                 ifelse(pval < 0.05, "*",
                                        ifelse(pval < 0.1, ".", ""))))
      coef[[" "]] <- asterisks

      # Print coefficients table
      cat("Coefficients:\n")
      print.data.frame(coef, digits = digits)
      cat("---\n")
      cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
      cat("\n\n")

      # Degrees of freedom
      cat("Residual standard error:",
          format(sqrt(sigmaHat), digits = digits), "on",
          df, "degrees of freedom")

      return(invisible(.self))
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

      suppressMessages( # without suppressing output of testthaat looks horrible
        devtools::use_package("digest")
      )

      # Check if it is NULL (never initialized)
      if (is.null(.self$cache[[methodName]]$hash)) {
        return(FALSE)
      }

      # Check if hash of current data, formula is the same as it was when cache
      # was computed
      currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
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

      suppressMessages( # without suppressing output of testthaat looks horrible
        devtools::use_package("digest")
      )

      # Calculate hash of the list with two objects
      # - formula
      # - data
      currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
      # Store hash in the cache list under the appropriate method
      .self$cache[[methodName]] <- list(
       hash = currentHash,
       value = value
      )

     return(invisible(currentHash))
    }
  )
)
