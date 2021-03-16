library(palmerpenguins)
penguins <- na.omit(penguins)

test_that("my_rf_cv output", {
  my_train_value <- my_rf_cv(5)
  expect_is(my_train_value, "numeric")
})
