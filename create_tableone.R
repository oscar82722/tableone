source('./src/stat_info.R')
source('./src/stat_test.R')
source('./src/type_setting.R')

create_tableone <- function(df, group, variable, type='type1'){
  
  tb_res <- data.table()
  for(var in variable){
    
    # step 1. get stat info
    tb_res_u <- get_stat_info_bygroup(df=df,
                                      var=var,
                                      group=group)
    
    # step 2. get stat test
    if(group == ''){
      stat_test_tb = ''
    }else{
      # clean na value
      df_temp <- df[,c(group, var),with=F]
      df_temp <- df_temp[complete.cases(df_temp)]
      
      # get test result
      stat_test_tb <- get_stat_test(df=df_temp,
                                    group_col=group,
                                    value_col=var)
    }

    # step 3. type_setting
    tb_res_u <- run_type_setting(var=var,
                                 stat_info_tb=tb_res_u,
                                 stat_test_tb=stat_test_tb,
                                 type=type)

    
    # combine
    tb_res <- rbind(tb_res, tb_res_u)
    tb_res <- replace(tb_res,is.na(tb_res), "")
  }
  
  return(tb_res)
}


