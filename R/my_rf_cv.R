#' Random Forest Cross-Validation
#'
#' A function to perform cross-validation classification using random forest.
#'
#' @param k Number of folders
#' @keywords prediction
#'
#' @return A numeric with the cross-validation error.
#'
#' @import randomForest
#' @examples
#' library(palmerpenguins)
#' penguins <- na.omit(penguins)
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  data_raw <- na.omit(penguins)
  n <- nrow(data_raw)
  folds <- sample(rep(1:k, length = n))
  data <- data.frame("x" = data_raw[,3:5], "y" = data_raw[6], "split" = folds)
  cv_err <- rep(NA, k)
  for(i in 1:k) {
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)
    model <- randomForest(body_mass_g ~ x.bill_length_mm + x.bill_depth_mm + x.flipper_length_mm, data = data_train, ntree = 100)
    predictions <- predict(model, data_test[,-4])

    cv_err[i] <- mean((predictions - data_test$body_mass_g)^2)
  }
  return(mean(cv_err))
}
