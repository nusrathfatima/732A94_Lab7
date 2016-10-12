#' @export

ridgereg <- function(formula, data, lambda) {
  ridgeregObject <- Ridgereg$new(formula = formula,
                           data = data,
                           lambda = lambda,
                           call = match.call())
return(invisible(ridgeregObject))
}
