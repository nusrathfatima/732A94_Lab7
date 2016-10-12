# Test input parameters --------------------------------------------------------
context("Test linreg inputs")
# Data frame with multicollinearity issues
dfMulticol <- data.frame(y = rnorm(3), x = rep(1,3), z = rep(2,3))
test_that("wrong parameters", {
  expect_error(linreg(function(x) {}, "data"),    "wrong parameters")
  expect_error(linreg("formula", function(x) {}), "wrong parameters")
  expect_error(linreg(X ~ Sepal.Width, data = iris), "variable(s) not in data",
               fixed = TRUE)
  expect_error(linreg(Sepal.Width ~ ., data = iris), "variable(s) not in data",
               fixed = TRUE)
  expect_error(linreg(x ~ y, data.frame(x = numeric(0), y = numeric(0))),
               "data must have >=1 cols/rows")
  expect_error(linreg(y ~ x + z, dfMulticol), "multicollinearity of regressors")
})


# Test output ------------------------------------------------------------------
context("Testing methods of Linreg class")
lmObject <-         lm(formula = mpg ~ hp, data = mtcars)
linregObject <- linreg(formula = mpg ~ hp, data = mtcars)
lmObjectNoConst <-         lm(formula = mpg ~ 0 + hp, data = mtcars)
linregObjectNoConst <- linreg(formula = mpg ~ 0 + hp, data = mtcars)
test_that("Same as in lm", {
  expect_that(linregObject$coef(),  equals(lmObject$coefficients))
  expect_that(linregObject$resid(), equals(lmObject$residuals))
  expect_that(linregObject$pred(),  equals(predict(lmObject)))
  # expect_that(linregObject$print(),  equals(print(lmObject)))
  # expect_that(linregObject$plot(),  equals(plot(lmObject)))
  # expect_that(linregObject$summary(),  equals(summary(lmObject)))
  expect_that(linregObjectNoConst$coef(),  equals(lmObjectNoConst$coefficients))
  expect_that(linregObjectNoConst$resid(), equals(lmObjectNoConst$residuals))
  expect_that(linregObjectNoConst$pred(),  equals(predict(lmObjectNoConst)))
  # expect_that(linregObjectNoConst$print(),  equals(print(lmObjectNoConst)))
  # expect_that(linregObjectNoConst$plot(),  equals(plot(lmObjectNoConst)))
  # expect_that(linregObjectNoConst$summary(),  equals(summary(lmObjectNoConst)))
})

# Compare results to lm function
lmObject <-         lm(formula = mpg ~ hp, data = mtcars)
linregObject <- linreg(formula = mpg ~ hp, data = mtcars)
test_that("Same as in lm", {
  expect_that(linregObject$coef(),  equals(lmObject$coefficients))
  expect_that(linregObject$resid(), equals(lmObject$residuals))
  expect_that(linregObject$pred(),  equals(predict(lmObject)))
  # expect_that(linregObject$print(),  equals(print(lmObject)))
  # expect_that(linregObject$plot(),  equals(plot(lmObject)))
  # expect_that(linregObject$summary(),  equals(lmObject(summary)))
})

# Test caching -----------------------------------------------------------------
context("Testing caching of Linreg class")
lmObject <-         lm(formula = mpg ~ hp, data = mtcars)
linregObject <- linreg(formula = mpg ~ hp, data = mtcars)

# 1st and second call return the same result
test_that("Cache: same results", {
  expect_that(linregObject$coef(),  equals(lmObject$coefficients))
  expect_that(linregObject$coef(),  equals(lmObject$coefficients))
  expect_that(linregObject$resid(), equals(lmObject$residuals))
  expect_that(linregObject$resid(), equals(lmObject$residuals))
  expect_that(linregObject$pred(),  equals(predict(lmObject)))
  expect_that(linregObject$pred(),  equals(predict(lmObject)))
})

# Check that object notices that formula/data has changed and recomputes
lmObject <- lm(formula = mpg ~ gear, data = mtcars)
linregObject$formula <- mpg ~ gear

test_that("Cache: spot changes", {
  expect_that(linregObject$coef(),  equals(lmObject$coefficients))
  expect_that(linregObject$resid(), equals(lmObject$residuals))
  expect_that(linregObject$pred(),  equals(predict(lmObject)))
})
