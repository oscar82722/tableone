source('./src/stat_info.R')
source('./src/stat_test.R')
source('./src/type_setting.R')


set.seed(123)
x1 <- c(rnorm(50, 0, 1), rnorm(50, 2, 1))
x2 <- c(rnorm(50, 0, 1), rnorm(50, 0, 1))
y1 <- c(rep('a', 50), rep('b', 50))
y2 <- c(rep('a', 50), rep('b', 50), rep('c', 50), rep('d', 50))
y3 <- c(rep('a', 3), rep('b', 97))
df <- data.table(x1, x2, y1, y2, y3)

group = 'y1'
variable = c('x1', 'x2', 'y1', 'y2', 'y3')




run_step_dict = list(
  'case1' = c(T,F,F),
  'case2' = c(T,T,T)
)


# check group element
group_element <- 'overall'
if(group != ''){
  g_element <- df[,unique(get(group))]
  group_element <- c(g_element, group_element)
}

# check run step
if(group == ''){
  run_step <- run_step_dict[['case1']]
}else{
  run_step <- run_step_dict[['case2']]
}


tb_res <- data.table()
for(var in variable){
  # step 1. get stat info
  if(run_step[1]){
    for(g in group_element){
      # selet data
      if(g == 'overall'){
        value <- df[,get(var)]
      }else{
        value <- df[get(group) == g,get(var)]
      }
      
      # calculate
      stat_info_u <- get_stat_info(x=value)
      colnames(stat_info_u)[2] <- g
      if(g == group_element[1]){
        tb_res_u <- stat_info_u
      }else{
        tb_res_u <- cbind(tb_res_u, stat_info_u[,g,with=F]) 
      }
    }
  }

  # step 2. get stat test
  if(run_step[2]){
    stat_test_tb <- get_stat_test(df,
                                  group_col=group,
                                  value_col=var)
  }

  # step 3. type_setting
  if(run_step[3]){
    tb_res_u <- type_setting_v1(var=var,
                               stat_info_tb=tb_res_u,
                               stat_test_tb=stat_test_tb)
  }

  # combine
  tb_res <- rbind(tb_res, tb_res_u)
}













