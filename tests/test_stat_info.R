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

  expect_equal(get_stat_info(x=x,run_stat_byclass_dict=run_stat_byclass_dict), res)
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
  
  expect_equal(get_stat_info(x=x,run_stat_byclass_dict=run_stat_byclass_dict), res)
})


test_that("stat_info_bygroup_numeric", {
  set.seed(123)
  x <- c(rnorm(99, 0, 1), NA)
  g <- c(rep('a',50), rep('b',50))
  df <- data.table(x,g)
  run_stat_byclass_dict = list(
    numeric = c('[Min,Max]', 'N/A')
  )
  
  # result
  Item <- c('[Min,Max]', 'N/A')
  col2 <- c('[-2.31,2.19]', '1(1%)')
  col3 <- c('[-1.97,2.17]', '0(0%)')
  col4 <- c('[-2.31,2.19]', '1(2%)')
  res <- data.table(Item, col2, col3, col4)
  colnames(res) <- c('Item', 'overall(N=100)', 'a(N=50)', 'b(N=50)')
  
  expect_equal(get_stat_info_bygroup(df=df,
                                     var='x',
                                     group='g',
                                     run_stat_byclass_dict=run_stat_byclass_dict), res)
})


test_that("stat_info_bygroup_character", {
  x <- c(rep('a',24), NA, rep('b',50), rep('c',25))
  g <- c(rep('a',50), rep('b',50))
  df <- data.table(x,g)
  
  # result
  Item <- c('a', 'b', 'c', 'N/A')
  col2 <- c('24(24.24%)', '50(50.51%)', '25(25.25%)', '1(1%)')
  col3 <- c('24(48.98%)', '25(51.02%)', '0(0%)', '1(2%)')
  col4 <- c('0(0%)', '25(50%)', '25(50%)', '0(0%)')
  res <- data.table(Item, col2, col3, col4)
  colnames(res) <- c('Item', 'overall(N=100)', 'a(N=50)', 'b(N=50)')
  
  expect_equal(get_stat_info_bygroup(df=df,
                                     var='x',
                                     group='g'), res)
})


test_that("stat_info_bygroup_nogroup", {
  x <- c(rep('a',24), NA, rep('b',50), rep('c',25))
  g <- c(rep('a',50), rep('b',50))
  df <- data.table(x,g)
  
  # result
  Item <- c('a', 'b', 'c', 'N/A')
  col2 <- c('24(24.24%)', '50(50.51%)', '25(25.25%)', '1(1%)')
  res <- data.table(Item, col2)
  colnames(res) <- c('Item', 'overall(N=100)')
  
  expect_equal(get_stat_info_bygroup(df=df,
                                     var='x',
                                     group=''), res)
})

