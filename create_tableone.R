source('./src/stat_info.R')
source('./src/stat_test.R')
source('./src/type_setting.R')

create_tableone <- function(df, group, variable, type='type1'){
  
  # check group element
  group_element <- 'overall'
  if(group != ''){
    g_element <- df[,unique(get(group))]
    group_element <- c(group_element, g_element)
  }
  
  tb_res <- data.table()
  for(var in variable){
    
    # step 1. get stat info
    for(g in group_element){
      # select data
      if(g == 'overall'){
        value <- df[,get(var)]
      }else{
        value <- df[get(group) == g,get(var)]
      }
      
      # size
      n <- length(value)
      # calculate
      stat_info_u <- get_stat_info(x=value)
      if(g == group_element[1]){
        colnames(stat_info_u)[2] <- g
        tb_res_u <- stat_info_u
        var_rank <- tb_res_u[,Item]
      }else{
        colnames(stat_info_u)[2] <- paste0(g,'(N=',n,')')
        tb_res_u <- merge(tb_res_u, stat_info_u, by='Item', all=T) 
      }
    }
    # order
    tb_res_u <- tb_res_u[order(match(Item, var_rank))]
    
    # fill na
    if(class(df[,get(var)]) == 'character'){
      tb_res_u = replace(tb_res_u,is.na(tb_res_u), "0(0%)")
    }

    # step 2. get stat test
    if(group == ''){
      stat_test_tb = ''
    }else{
      # clean na value
      df_temp <- df[,c(group, var),with=F]
      df_temp <- df_temp[complete.cases(df_temp)]
      
      # get test result
      stat_test_tb <- get_stat_test(df_temp,
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
  
  # overall size
  overall_n <- df[,.N]
  colnames(tb_res)[length(colnames(tb_res))] <- paste0('overall(N=', overall_n, ')')
  
  return(tb_res)
}


