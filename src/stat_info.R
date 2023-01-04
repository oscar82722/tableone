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


