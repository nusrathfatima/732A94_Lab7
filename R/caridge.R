#' Caret Ridge BostonHousing
#'
#' Use \code{caridge} to perform ridge regression.
#'
#' @param BostonHousing only works with this data set at the moment.
#'
#' @return \code{caridge} returns an object of class "\code{ridge}".
#'
#' @seealso \code{\link{lm}}, \code{\link{class}}, \code{\link{formula}}
#'
#' @examples
#' caridge(BostonHousing)
#'
#' \dontrun{
#' caridge(TRUE, TRUE)
#' }
#' @export
caridge<-function(X){
  data("BostonHousing")
  dat <- X
  trainIndex <- caret::createDataPartition(dat$age, p = .75,
                                           list = FALSE,
                                           times= 1)
  datTrain <- dat[trainIndex, ]
  datTest <- dat[-trainIndex, ]
  res <- c(datTrain, datTest)
  fitControl <- caret::trainControl(method = "cv",
                             number = 10)
  # Set seq of lambda to test
  lambdaGrid <- expand.grid(lambda = c(0,.01,.02,.03,.04))
  ridge <- caret::train(crim~.,
                 data = datTrain,
                 method='ridge',
                 trControl = fitControl,
                                tuneGrid = lambdaGrid,
                 preProcess=c('center', 'scale')
  )
  predict(ridge$finalModel, type='coef', mode='norm')$coefficients[13,]
  ridge.pred <- predict(ridge, datTest)
  avgErrror<-2*sqrt(mean(ridge.pred - datTest$crim)^2)
  return(ridge)
}
