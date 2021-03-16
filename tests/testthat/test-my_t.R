set.seed(302)
a <- rbinom(100, size = 10, prob = 0.2)

test_that("my_t.test works mathematically with two.sided",{
  my_value <- my_t.test(a, "two.sided", 0.8 )
  true_value <- t.test(a, mu = 0.8, alternative = "two.sided")
  expect_true(my_value$test_stat == true_value$statistic)
  expect_true(my_value$df == true_value$parameter)
  expect_true(my_value$p_val == true_value$p.value)
})

test_that("my_t.test works mathematically with greater",{
  my_value <- my_t.test(a, "greater", 0.8 )
  true_value <- t.test(a, mu = 0.8, alternative = "greater")
  expect_true(my_value$test_stat == true_value$statistic)
  expect_true(my_value$df == true_value$parameter)
  expect_true(my_value$p_val == true_value$p.value)
})

test_that("my_t.test works mathematically with less",{
  my_value <- my_t.test(a, "less", 0.8 )
  true_value <- t.test(a, mu = 0.8, alternative = "less")
  expect_true(my_value$test_stat == true_value$statistic)
  expect_true(my_value$df == true_value$parameter)
  expect_true(my_value$p_val == true_value$p.value)
})

test_that("my_t.test with error alternatives", {
  expect_error(my_t.test(a, "sample",  0.8))
})
