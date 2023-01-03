source("../src/stat_info.R", chdir = TRUE)
library(testthat)


test_that("mean_func", {
  x <- c(1,1,2,2,3,4,5,NA)
  expect_equal(mean_func(x), '2.57(1.51)')
})

test_that("median_func", {
  x <- c(1,1,2,2,3,4,5,NA)
  expect_equal(median_func(x), '2(2)')
})

test_that("range_func", {
  x <- c(1,1,2,2,3,4,5,NA)
  expect_equal(range_func(x), '[1,5]')
})

test_that("count_na_func", {
  x <- c(1,1,2,2,3,4,5,NA)
  expect_equal(count_na_func(x), '1(12.5%)')
})

test_that("percent_func", {
  x <- c('a', 'a', 'b', 'b', NA)
  res <- c('2(50%)', '2(50%)')
  names(res) <- c('a', 'b')
  expect_equal(percent_func(x), res)
})


test_that("stat_info_numeric", {
  x <- c(1,1,2,2,3,4,5,NA)
  run_stat_byclass_dict = list(
    numeric = c('[Min,Max]', 'N/A')
    )
  
  # result
  Item <- c('[Min,Max]', 'N/A')
  res <- c('[1,5]', '1(12.5%)')
  res <- data.table(Item, res)

  expect_equal(stat_info(x=x,run_stat_byclass_dict=run_stat_byclass_dict), res)
})


test_that("stat_info_character", {
  
  run_stat_byclass_dict = list(
    character = c('percent_by_element', 'N/A')
  )
  
  x <- c('a', 'a', 'b', 'b', NA)
  
  # result
  Item <- c('a', 'b', 'N/A')
  res <- c('2(50%)', '2(50%)', '1(20%)')
  res <- data.table(Item, res)
  
  expect_equal(stat_info(x=x,run_stat_byclass_dict=run_stat_byclass_dict), res)
})





