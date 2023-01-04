source("../src/stat_test.R", chdir = TRUE)
library(testthat)

set.seed(123)
x1 <- c(rnorm(50, 0, 1), rnorm(50, 2, 1))
x2 <- c(rnorm(50, 0, 1), rnorm(50, 0, 1))
y1 <- c(rep('a', 50), rep('b', 50))
y2 <- c(rep('a', 50), rep('b', 50), rep('c', 50), rep('d', 50))
y3 <- c(rep('a', 3), rep('b', 97))
df <- data.table(x1, x2, y1, y2, y3)


test_that("customize_aov", {
  f <- as.formula('x1 ~ y1')
  expect_equal(customize_aov(f, df), list(p.value=1.017678e-38))
})


test_that("customize_chi_test", {
  f <- as.formula('y3 ~ y1')
  expect_equal(names(customize_chi_test(f, df)),'p.value')
})


test_that("customize_fisher_test", {
  f <- as.formula('y3 ~ y1')
  expect_equal(names(customize_fisher_test(f, df)),'p.value')
})


test_that("judge_test_name_case1", {
  expect_equal(judge_test_name(df, 'y1', 'x1'), c("t_test","wilcox_test"))
})


test_that("judge_test_name_case2", {
  expect_equal(judge_test_name(df, 'y1', 'y3'), c("fisher_test"))
})


test_that("judge_test_name_case3", {
  expect_equal(judge_test_name(df, 'y1', 'y2'), c("chi_test"))
})


test_that("stat_test", {
  expect_equal(colnames(get_stat_test(df, 'y1', 'x1')), c("method", "p.value"))
})


