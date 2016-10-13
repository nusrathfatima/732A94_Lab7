
  data("BostonHousing")
  dat <- BostonHousing
  trainIndex <- caret::createDataPartition(dat$age, p = .75,
                                           list = FALSE,
                                           times= 1)
  datTrain <- dat[trainIndex, ]
  datTest <- dat[-trainIndex, ]
  res <- c(datTrain, datTest)
  fitControl <- trainControl(method = "cv",
                             number = 10)
  # Set seq of lambda to test
  lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))
  ridge <- train(crim~.,
                 data = datTrain,
                 method='ridge',
                 trControl = fitControl,
                                tuneGrid = lambdaGrid,
                 preProcess=c('center', 'scale')
  )
  predict(ridge$finalModel, type='coef', mode='norm')$coefficients[13,]
  ridge.pred <- predict(ridge, datTest)
  avgErrror<-2*sqrt(mean(ridge.pred - datTest$crim)^2)
  ridge
