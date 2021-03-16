data(mtcars)
mtcars <- na.omit(mtcars)
rawlm <- lm(mpg ~ hp + wt, data = mtcars)
my_lm_value <- my_lm(mpg ~ hp + wt, data = mtcars)
true_lm_value <- summary(rawlm)
test_that("my_lm works mathmetically", {
  my_lm_test <- as.vector(round(my_lm_value,3) == round(true_lm_value$coefficients, 3))
  if(all(my_lm_test) == TRUE) {
    my_lm_test <- TRUE
  }
  expect_true(my_lm_test)
})
