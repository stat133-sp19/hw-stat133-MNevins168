context("testing all functions in bin_statmanips.R")
library(testthat)

source('binomial/R/bin_statmanips.R')

test_that("check_prob fails with prob greater than 1, less than 0, or with a non-numeric input", {
  expect_error(check_prob(4))
  expect_error(check_prob(-2.1))
  expect_error(check_prob("one half"))
})

test_that("check_trials fails for negative values, non-integer values, and non-numeric values", {
  expect_error(check_trials(-2))
  expect_error(check_trials(2.1))
  expect_error(check_trials("two"))
})

test_that("check_success fails for negative values, non-integer values, non-numeric values, and values higher than trials: works with success as a vector", {
  expect_error(check_success(-2, 3))
  expect_error(check_success(2.1, 3))
  expect_error(check_success("two", 3))
  expect_error(check_success(4, 3))
  expect_true(check_success(1:3, 4))
})






test_that("aux_mean works with normal values, but does not reject non-integer trials or prob and/or trials less than zero", {
  expect_equal(aux_mean(10,0.3), 3)
  expect_equal(aux_mean(10,1.3), 13)
  expect_equal(aux_mean(-10,0.3), -3)
  expect_equal(aux_mean(3.6,0.3), 1.08)
})

test_that("aux_variance works with normal values, but does not reject non-integer trials or prob and/or trials less than zero", {
  expect_equal(aux_variance(10,0.3), 2.1)
  expect_equal(aux_variance(10,0), 0)
  expect_equal(aux_variance(0,0.3), 0)
  expect_equal(aux_variance(10,1),0)
})

test_that("aux_mode works with normal values, and gives two values if np + p is an integer", {
  expect_equal(aux_mode(10,0.3), 3)
  expect_equal(aux_mode(3,0.5), c(2,1))
  expect_equal(aux_mode(0,0.8), 0)
})

test_that("aux_skewness works with normal values, but gives Inf if given a zero due to dividing by zero", {
  expect_equal(aux_skewness(10,0.3), (1-2*0.3)/(sqrt(10*0.3*(1-0.3))))
  expect_equal(aux_skewness(0,0.3),Inf)
  expect_equal(aux_skewness(10,0),Inf)
})

test_that("aux_kurtosis works with normal values, but gives Inf if given a zero due to dividing by zero", {
  expect_equal(aux_kurtosis(10,0.3), (1-6*0.3*(1-0.3))/(10*0.3*(1-0.3)))
  expect_equal(aux_kurtosis(0,0.3), -Inf)
  expect_equal(aux_kurtosis(10,0), Inf)
})







test_that("bin_choose does n choose k with valid values, can have k as a vector, but rejects if k is larger than n.", {
  expect_equal(bin_choose(4,3),4)
  expect_equal(bin_choose(4,c(0,2,4)), c(1,6,1))
  expect_error(bin_choose(3,5))
  expect_error(bin_choose(4,c(1,3,5)))
  })

test_that("bin_probability finds the correct value for given trials, prob, and success, but fails if any of those are not valid", {
  expect_equal(bin_probability(2,5,0.5), 0.3125)
  expect_error(bin_probability("two",5,0.5))
  expect_error(bin_probability(2,-5,0.5))
  expect_error(bin_probability(2,5,1.2))
})

test_that("bin_distribution creates an object of type bindis, and has a dataframe inside of name datafr which fails if trials or prob do not follow the proper criteria", {
  expect_equal(class(bin_distribution(5,0.5)), c("bindis", "data.frame"))
  expect_false(nrow(bin_distribution(5,0.5)$datafr) == 0 | ncol(bin_distribution(5,0.5)$datafr) != 2)
  expect_error(bin_distribution(5,-0.5))
  expect_error(bin_distribution(-5,0.5))
})

test_that("bin_cumulative creates an object of type bindis, and has a dataframe inside of name datafr which fails if trials or prob do not follow the proper criteria", {
  expect_equal(class(bin_cumulative(5,0.5)), c("bincum", "data.frame"))
  expect_false(nrow(bin_cumulative(5,0.5)$datafr) == 0 | ncol(bin_cumulative(5,0.5)$datafr) != 3)
  expect_error(bin_cumulative(5,-0.5))
  expect_error(bin_cumulative(-5,0.5))
})

