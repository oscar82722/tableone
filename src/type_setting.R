library(data.table)



type_setting_v1 <- function(var, stat_info_tb, stat_test_tb){
  '
  sample_numeric :
  |var |Item   | G1 | G2| G3| ... | p.value| method | overall|
  |v1  |       |    |   |   |     |        |        |        |
  |    |mean   |xxx | xx| xx| ... | <0.001 |        | xxx    |
  |    |median |xxx | xx| xx| ... | 0.01   |        | xxx    |
  
  sample character :
  |var |Item   | G1 | G2| G3| ... | p.value| method | overall|
  |v2  |       |    |   |   | ... | <0.001 |chi-test|        |
  |    | g1    |xxx | xx| xx| ... |        |        | xxx    |
  |    | g2    |xxx | xx| xx| ... |        |        | xxx    |
  |    | g3    |xxx | xx| xx| ... |        |        | xxx    |
  '
  
  # setting
  map_tb <- data.table(Item = c('Mean(sd)', 'Median(IQR)', 'Mean(sd)', 'Median(IQR)'),
                       method = c('t_test', 'wilcox_test', 'aov_test', 'ks_test'))
  p.value_col <- c('chi_test', 'fisher_test')
  
  # check final column
  var_element <- colnames(stat_info_tb)[!(colnames(stat_info_tb) %in% c('overall'))]
  col_final <- c('var', var_element, 'p.value', 'method', 'overall')
  
  # first part (title)
  p.value <- stat_test_tb[ method %in% p.value_col, p.value]
  method <- stat_test_tb[ method %in% p.value_col,  method]
  first_row <- data.table(var,
                          p.value= ifelse(length(p.value) == 0, NA, p.value),
                          method = ifelse(length(method) == 0, NA, method))
  
  # second part (info and test)
  stat_info_tb[,obs:=1:.N]
  info_and_test <- merge(stat_test_tb, map_tb, all.x=T)
  info_and_test <- merge(stat_info_tb, info_and_test, by='Item', all.x=T)
  info_and_test <- info_and_test[order(obs)]
  
  # combind 
  result <- rbind(first_row, info_and_test, fill=TRUE)
  result <- result[,col_final,with=F]
  
  return(result)
}














