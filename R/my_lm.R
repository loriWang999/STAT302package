#' Linear  Model
#'
#' This function is used to fit linear models.
#'
#' @param formula Class object of \code{formula}, a symbolic
#'    description of the model to be fitted.
#' @param data Data frame of input.
#' @keywords inference
#'
#' @return A table with rows for each coefficient and columns
#'    for the \code{Estimate}, \code{Std.Error}, \code{t value},
#'    and \code{Pr(>|t|)}.
#'
#' @import tibble
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
#'
my_lm <- function(formula, data) {
  # Extract X and Y as a matrix
  X <- model.matrix(formula, data)
  Y <- model.response(model.frame(formula, data))

  # Calculate coefficients beta
  coeff <- (solve(t(X) %*% X) %*% t(X) %*% Y)

  # df = sample size - number of covariates
  df = nrow(data) - ncol(X)

  # Calculate standard error
  variance <- sum((Y - (X %*% coeff))^2/df)
  se <- diag(sqrt(variance * solve(t(X) %*% X)))

  # Calculate test statistic
  t_value <- coeff/se

  # p value for two sides
  two_t.test <- 2*pt(abs(t_value), df, lower.tail = FALSE)

  # The output for my_lm()
  result <- cbind(coeff, se, t_value, two_t.test)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)" )

  return(as.table(result))
}
