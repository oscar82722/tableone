library(data.table)

mean_func <- function(x){
  m <- mean(x, na.rm = T)
  s <- sd(x, na.rm = T)
  res <- paste0(round(m,2),'(',round(s,2),')')
  return(res)
}

median_func <- function(x){
  m <- median(x, na.rm = T)
  s <- IQR(x, na.rm = T)
  res <- paste0(round(m,2),'(',round(s,2),')')
  return(res)
}

range_func <- function(x){
  mi <- min(x, na.rm = T)
  mx <- max(x, na.rm = T)
  res <- paste0('[', round(mi,2), ',', round(mx,2), ']')
  return(res)
}

count_na_func <- function(x){
  tot <- length(x)
  na_count <- sum(is.na(x))
  p <- round((na_count / tot), 4)*100
  res <- paste0(na_count, '(', p, '%)')
  return(res)
}

percent_func <- function(x){
  tot <- length(x[!is.na(x)])
  count_per_element <- table(x)
  percent_per_element <- round((count_per_element / tot), 4)*100
  res <- paste0(count_per_element, '(', percent_per_element, '%)')
  names(res) <- names(percent_per_element)
  return(res)
}

stat_info_func_dict = list(
  `Mean(sd)` = mean_func,
  `Median(IQR)` = median_func,
  `[Min,Max]` = range_func,
  `N/A` = count_na_func,
  percent_by_element = percent_func
)


get_stat_info <- function(x,
                          run_stat_byclass_dict = list(
                          numeric = c('Mean(sd)', 'Median(IQR)', '[Min,Max]', 'N/A'),
                          character = c('percent_by_element', 'N/A'))
                      ){
  
  variable_class = class(x)
  stat_info_v <- run_stat_byclass_dict[[variable_class]]
  
  res_tb <- data.table()
  for(stat_info_name in stat_info_v){
    func_ <- stat_info_func_dict[[stat_info_name]]
    res <- func_(x)
    if(is.null(names(res))){
      Item <- stat_info_name
    }else{
      Item <- names(res)
    }
    res_temp <- data.table(Item, res)
    res_tb <- rbind(res_tb, res_temp)
  }
  return(res_tb)
}


get_stat_info_bygroup <- function(df,
                                  var,
                                  group='',
                                  run_stat_byclass_dict=list(
                                    numeric = c('Mean(sd)', 'Median(IQR)', '[Min,Max]', 'N/A'),
                                    character = c('percent_by_element', 'N/A'))
                                  ){
  
  group_element <- 'overall'
  if(group != ''){
    df <- df[,c(var, group), with=F]
    colnames(df) <- c('value_col', 'group_col')
    g_element <- df[,unique(group_col)]
    group_element <- c(group_element, g_element)
  }else{
    df <- df[,c(var), with=F]
    colnames(df) <- c('value_col')
  }
  
  for(g in group_element){
    # select data
    if(g == 'overall'){
      value <- df[,value_col]
    }else{
      value <- df[group_col == g, value_col]
    }
    # calculate
    stat_info_u <- get_stat_info(x=value,
                                 run_stat_byclass_dict=run_stat_byclass_dict)
    colnames(stat_info_u)[2] <- paste0(g, '(N=', length(value), ')')
    
    if(g == group_element[1]){
      tb_res_u <- stat_info_u
      var_rank <- tb_res_u[,Item]
    }else{
      tb_res_u <- merge(tb_res_u, stat_info_u, by='Item', all=T) 
    }
  }
  
  # order
  tb_res_u <- tb_res_u[order(match(Item, var_rank))]
  
  # fill na
  if(class(df[,value_col]) == 'character'){
    tb_res_u = replace(tb_res_u, is.na(tb_res_u), "0(0%)")
  }
  
  return(tb_res_u)
}

