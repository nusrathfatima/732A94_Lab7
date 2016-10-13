# Test input parameters --------------------------------------------------------
context("Test ridgereg inputs")
# Data frame with multicollinearity issues
dfMulticol <- data.frame(y = rnorm(3), x = rep(1,3), z = rep(2,3))
test_that("wrong parameters", {
  expect_error(ridgereg(function(x) {}, "data"),    "wrong parameters")
  expect_error(ridgereg("formula", function(x) {}), "wrong parameters")
  expect_error(ridgereg(X ~ Sepal.Width, data = iris), "variable(s) not in data",
               fixed = TRUE)
  expect_error(ridgereg(Sepal.Width ~ ., data = iris), "variable(s) not in data",
               fixed = TRUE)
  expect_error(ridgereg(x ~ y, data.frame(x = numeric(0), y = numeric(0))),
               "data must have >=1 cols/rows")
  expect_error(ridgereg(y ~ x + z, dfMulticol), "multicollinearity of regressors")
})


# # Test output ------------------------------------------------------------------
# context("Testing methods of ridgereg class")
# lmObject <-         lm(formula = mpg ~ hp, data = mtcars)
# ridgeregObject <- ridgereg(formula = mpg ~ hp, data = mtcars)
# lmObjectNoConst <-         lm(formula = mpg ~ 0 + hp, data = mtcars)
# ridgeregObjectNoConst <- ridgereg(formula = mpg ~ 0 + hp, data = mtcars)
# test_that("Same as in lm", {
#   expect_that(ridgeregObject$coef(),  equals(lmObject$coefficients))
#   expect_that(ridgeregObject$resid(), equals(lmObject$residuals))
#   expect_that(ridgeregObject$pred(),  equals(predict(lmObject)))
#   # expect_that(ridgeregObject$print(),  equals(print(lmObject)))
#   # expect_that(ridgeregObject$plot(),  equals(plot(lmObject)))
#   # expect_that(ridgeregObject$summary(),  equals(summary(lmObject)))
#   expect_that(ridgeregObjectNoConst$coef(),  equals(lmObjectNoConst$coefficients))
#   expect_that(ridgeregObjectNoConst$resid(), equals(lmObjectNoConst$residuals))
#   expect_that(ridgeregObjectNoConst$pred(),  equals(predict(lmObjectNoConst)))
#   # expect_that(ridgeregObjectNoConst$print(),  equals(print(lmObjectNoConst)))
#   # expect_that(ridgeregObjectNoConst$plot(),  equals(plot(lmObjectNoConst)))
#   # expect_that(ridgeregObjectNoConst$summary(),  equals(summary(lmObjectNoConst)))
# })
#
# # Compare results to lm function
# lmObject <-         lm(formula = mpg ~ hp, data = mtcars)
# ridgeregObject <- ridgereg(formula = mpg ~ hp, data = mtcars)
# test_that("Same as in lm", {
#   expect_that(ridgeregObject$coef(),  equals(lmObject$coefficients))
#   expect_that(ridgeregObject$resid(), equals(lmObject$residuals))
#   expect_that(ridgeregObject$pred(),  equals(predict(lmObject)))
#   # expect_that(ridgeregObject$print(),  equals(print(lmObject)))
#   # expect_that(ridgeregObject$plot(),  equals(plot(lmObject)))
#   # expect_that(ridgeregObject$summary(),  equals(lmObject(summary)))
# })
