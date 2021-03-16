#' T-Test function
#'
#' This function performs one sample t-test on vectors of data.
#'
#' @param x Non-empty numeric vector of data values.
#' @param alternative String to specify the alternative hypothesis,
#'   must be one of \code{two.sided}, \code{greater} or \code{less}.
#' @param mu Number indicating the true value of the mean.
#' @keywords inference
#'
#' @return List to show the value of the \code{t-statistic}, \code{the degrees} of
#'   freedom, the \code{alternative} option and the \code{p-value} of the test.
#'
#'
#' @examples
#' a <- rbinom(100, size = 10, prob = 0.2)
#' my_t.test(a, "greater", 0.8)
#'
#' @export
my_t.test <- function(x, alternative, mu) {

  mu_0 <- mu

  est <- mean(x)
  df <- length(x) - 1
  se <- sd(x)/sqrt(length(x))
  t_obs <- (est - mu_0) / se

  # Calculate the p_value based on sides.
  if(alternative == "less") {
    p_value <- pt(q = t_obs, df)
  }else if (alternative == "greater") {
    p_value <- pt(q = t_obs, df, lower.tail = FALSE)
  } else if(alternative == "two.sided") {
    p_value <- 2*pt(q = abs(t_obs), df, lower.tail = FALSE)
  }else {
    stop('Alternative hypothesis only accept two.sided, less, or greater.')
  }

  # The output of my_t.test()
  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_value)

  return(result)
}
