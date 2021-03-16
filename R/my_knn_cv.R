#' K-Nearest Neighbors Cross-Validation
#'
#' This function use k-nearest neighbbour cross-validatory classfication for trainig set.
#'
#' @param train Data frame of training set from input.
#' @param cl True class value of training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list with \code{class} a vector of the predicted class
#'   for all observations, and \code{cv_err} the cross-validation misclassification error.
#'
#' @import class
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#' @importFrom dplyr filter mutate
#'
#' @examples
#'
#' penguins <- na.omit(my_penguins)
#' my_knn_cv(penguins[, 3:6], penguins$species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n <- nrow(train)
  folds <- sample(rep(1:k_cv, length = n))
  train <- data.frame("x" = train, "y" = cl, "split" = folds)
  train <- na.omit(train)
  predict_err <- rep(NA, k_cv)
  for (i in 1:k_cv) {
    data_train <- train%>% filter (split != i)
    data_test <- train%>% filter (split == i)
    class_predict <- knn(train = data_train[,1:4], test = data_test[,1:4], cl = data_train$y, k = k_nn)
    predict_err[i] <- sum(class_predict != data_test$y)/nrow(data_test)
  }
  class <- knn(train = train[,1:4], test = train[,1:4], cl = train$y, k = k_nn)
  cv_err <- mean(predict_err)
  return(list(class, cv_err))
}
