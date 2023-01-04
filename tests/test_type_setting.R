source("../src/type_setting.R", chdir = TRUE)
library(testthat)


test_that("type_setting_v1_numeric", {
  
  Item <- c('Mean(sd)', 'Median(IQR)', '[Min,Max]', 'N/A')
  res1 <- c('1', '2', '3', '4')
  res2 <- c('21', '22', '23', '24')
  overall <- c('o1', 'o2', 'o3', 'o4')
  stat_info_tb <- data.table(Item, res1, res2, overall)
  
  
  method <- c('t_test', 'wilcox_test')
  p.value <- c(0.01, '<0.001')
  stat_test_tb <- data.table(method, p.value)
  var <- 'v1'
  
  res <- data.table(var = c(var, NA, NA, NA, NA),
                   Item = c(NA, 'Mean(sd)', 'Median(IQR)', '[Min,Max]', 'N/A'),
                   res1 = c(NA, '1', '2', '3', '4'),
                   res2 = c(NA, '21', '22', '23', '24'),
                   p.value = c(NA, "0.01", "<0.001", NA, NA),
                   method = c(NA, 't_test', 'wilcox_test', NA, NA),
                   overall =c(NA, 'o1', 'o2', 'o3', 'o4'))
  
  expect_equal(type_setting_v1(var, stat_info_tb, stat_test_tb), res)
  
})

test_that("type_setting_v1_character", {
  Item <- c('g1', 'g2', 'g3', 'N/A')
  res1 <- c('1', '2', '3', '4')
  res2 <- c('21', '22', '23', '24')
  overall <- c('o1', 'o2', 'o3', 'o4')
  stat_info_tb <- data.table(Item, res1, res2, overall)
  
  
  method <- c('fisher_test')
  p.value <- c('<0.001')
  stat_test_tb <- data.table(method, p.value)
  var <- 'v1'
  
  res <- data.table(var = c(var, NA, NA, NA, NA),
                    Item = c(NA, 'g1', 'g2', 'g3', 'N/A'),
                    res1 = c(NA, '1', '2', '3', '4'),
                    res2 = c(NA, '21', '22', '23', '24'),
                    p.value = c('<0.001', NA, NA, NA, NA),
                    method = c('fisher_test', NA, NA, NA, NA),
                    overall =c(NA, 'o1', 'o2', 'o3', 'o4'))
  
  expect_equal(type_setting_v1(var, stat_info_tb, stat_test_tb), res)
})
