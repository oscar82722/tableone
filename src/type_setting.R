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
  
  sample none stat_test_tb :
  |var | overall|
  |v2  |        |
  |    | xxx    |
  |    | xxx    |
  |    | xxx    |
  '
  
  # setting
  map_tb <- data.table(Item = c('Mean(sd)', 'Median(IQR)', 'Mean(sd)', 'Median(IQR)'),
                       method = c('t_test', 'wilcox_test', 'aov_test', 'ks_test'))
  p.value_col <- c('chi_test', 'fisher_test')
  
  # check final column
  overall_col <- colnames(stat_info_tb)[grepl('overall', colnames(stat_info_tb))]
  var_element <- colnames(stat_info_tb)[!(colnames(stat_info_tb) %in% overall_col)]
  
  if(is.character(stat_test_tb)){
    col_final <- c('var', overall_col)
    first_row <- data.table(var)
    info_and_test <- stat_info_tb
  }else{
    col_final <- c('var', var_element, 'p.value', 'method', overall_col, 'smd')
    # first part (title)
    p.value <- stat_test_tb[ method %in% p.value_col, p.value]
    method <- stat_test_tb[ method %in% p.value_col,  method]
    smd_v <- stat_test_tb[ method == 'smd',  p.value]
    first_row <- data.table(var,
                            p.value= ifelse(length(p.value) == 0, NA, p.value),
                            method = ifelse(length(method) == 0, NA, method),
                            smd = smd_v)
    # second part (info and test)
    stat_info_tb[,obs:=1:.N]
    info_and_test <- merge(stat_test_tb, map_tb, all.x=T)
    info_and_test <- merge(stat_info_tb, info_and_test, by='Item', all.x=T)
    info_and_test <- info_and_test[order(obs)]
  }
  
  # combine
  result <- rbind(first_row, info_and_test, fill=TRUE)
  result <- result[,col_final,with=F]
  
  return(result)
}



type_setting_dict = list(
  type1 = type_setting_v1
)



run_type_setting <- function(var, stat_info_tb, stat_test_tb, type='type1'){
  func_ = type_setting_dict[[type]]
  result = func_(var=var,
                 stat_info_tb=stat_info_tb,
                 stat_test_tb=stat_test_tb)
  return(result)
}








