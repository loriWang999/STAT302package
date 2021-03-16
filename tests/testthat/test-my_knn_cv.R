library(palmerpenguins)
penguins <- na.omit(penguins)

test_that ("my_knn_cv output", {
  my_train_value <- my_knn_cv(penguins[, 3:6], penguins$species, 1, 5)
  expect_is(my_train_value, "list")
})
