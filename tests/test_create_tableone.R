source("../create_tableone.R", chdir = TRUE)
library(testthat)

set.seed(123)
x1 <- c(rnorm(50, 0, 1), rnorm(50, 2, 1))
x2 <- c(rnorm(99, 2, 0.5),NA)
y1 <- c(rep('a', 50), rep('b', 50))
y2 <- c(rep('a', 50), rep('b', 50), rep('c', 50), rep('d', 50))
y3 <- c(rep('a', 3), rep('b', 97))
y4 <- c(rep(c('a', 'b'),25), rep(c('a', 'b'),24), NA, NA)
df <- data.table(x1, x2, y1, y2, y3, y4)


test_that("create_tableone_group=None", {
  var <- c('x1', '', '', '', '',
           'x2', '', '', '', '')
  overall <- c('', '1.09(1.4)', '1.22(2.29)', '[-1.97,4.19]', '0(0%)',
               '', '1.95(0.48)', '1.89(0.64)', '[0.97,3.62]', '2(1%)')
  res <- data.table(var, overall)
  colnames(res)[2] <- 'overall(N=200)'
  expect_equal(
    create_tableone(df=df,
                    group='',
                    variable = c('x1','x2')),
    res)
})


test_that("create_tableone_group_two", {
  var <- c('x2', '', '', '', '',
           'y4', '', '', '')
  Item <- c('' , 'Mean(sd)', 'Median(IQR)', '[Min,Max]', 'N/A',
            '', 'a', 'b', 'N/A')
  a <-    c('' , '1.87(0.49)', '1.85(0.63)', '[0.97,3.05]', '0(0%)',
            '', '50(50%)', '50(50%)', '0(0%)')
  b <- c('', '2.03(0.46)', '1.98(0.52)', '[1.34,3.62]', '2(2%)',
         '', '48(50%)', '48(50%)', '4(4%)')
  p.value <- c('', '0.02', '0.019', '', '',
               '1', '', '', '')
  
  method <- c('', 't_test', 'wilcox_test', '', '',
              'chi_test', '', '', '')

  overall <- c('', '1.95(0.48)', '1.89(0.64)', '[0.97,3.62]', '2(1%)',
               '', '98(50%)', '98(50%)', '4(2%)')

  res <- data.table(var, Item, a, b, p.value, method, overall)
  colnames(res) <- c('var', 'Item', 'a(N=100)', 'b(N=100)', 'p.value', 'method',
                     'overall(N=200)')
  
  expect_equal(
    create_tableone(df=df,
                    group='y1',
                    variable = c('x2', 'y4')),
    res)
})


test_that("create_tableone_group_multi", {
  var <- c('x1', '', '', '', '',
           'y3', '', '', '')
  Item <- c('' , 'Mean(sd)', 'Median(IQR)', '[Min,Max]', 'N/A',
            '', 'a', 'b', 'N/A')
  a <-    c('' , '0.03(0.93)', '-0.07(1.26)', '[-1.97,2.17]', '0(0%)',
            '', '3(6%)', '47(94%)', '0(0%)')
  b <- c('', '2.15(0.91)', '2.15(0.99)', '[-0.31,4.19]', '0(0%)',
         '', '0(0%)', '50(100%)', '0(0%)')
  
  c <- c('', '0.03(0.93)', '-0.07(1.26)', '[-1.97,2.17]', '0(0%)',
         '', '3(6%)', '47(94%)', '0(0%)')
  
  d <- c('', '2.15(0.91)', '2.15(0.99)', '[-0.31,4.19]', '0(0%)',
         '', '0(0%)', '50(100%)', '0(0%)')
  
  p.value <- c('', '<0.001', '<0.001', '', '',
               '0.085', '', '', '')
  
  method <- c('', 'aov_test', 'ks_test', '', '',
              'fisher_test', '', '', '')
  
  overall <- c('', '1.09(1.4)', '1.22(2.29)', '[-1.97,4.19]', '0(0%)',
               '', '6(3%)', '194(97%)', '0(0%)')
  
  res <- data.table(var, Item, a, b, c, d, p.value, method, overall)
  colnames(res) <- c('var', 'Item', 'a(N=50)', 'b(N=50)', 'c(N=50)',
                     'd(N=50)', 'p.value', 'method', 'overall(N=200)')
  
  expect_equal(
    create_tableone(df=df,
                    group='y2',
                    variable = c('x1', 'y3')),
    res)
})



