library(data.table)


customize_aov <- function(formula, df){
  test_res <- aov(formula, df)
  p <- summary(test_res)[[1]]$`Pr(>F)`[1]
  res <- list(p.value = p)
  return(res)
}

customize_chi_test <- function(formula, df){
  col <- as.character(formula)[2:3]
  tb <- table(df[,col, with=F])

  set.seed(123)
  test_res <- chisq.test(tb, simulate.p.value = T)
  p <- test_res$p.value
  res <- list(p.value = p)
  return(res)
}


customize_fisher_test <- function(formula, df){
  col <- as.character(formula)[2:3]
  tb <- table(df[,col, with=F])
  test_res <- fisher.test(tb)
  p <- test_res$p.value
  res <- list(p.value = p)
  return(res)
}

test_dict = list(
  t_test = t.test,
  wilcox_test = wilcox.test,
  aov_test = customize_aov,
  ks_test = kruskal.test,
  chi_test = customize_chi_test,
  fisher_test = customize_fisher_test
)


judge_test_name <- function(df, group_col, value_col){
  
  value_class = class(df[, get(value_col)])
  
  test_func <- c()
  
  if(value_class == 'numeric'){
    n_group <- df[,length(unique(get(group_col)))]
    
    if(n_group == 2){
      test_func <- c(test_func, 't_test', 'wilcox_test') 
    }
    
    if(n_group > 2){
      test_func <- c(test_func, 'aov_test', 'ks_test') 
    }
  }
  
  if(value_class == 'character'){
    tb1 <- df[,table(get(group_col), get(value_col))]
    set.seed(123)
    c.test <- chisq.test(tb1, simulate.p.value = T)
    
    if(sum(c.test$expected < 5) >  length(tb1)*0.2){
      test_func <- c(test_func, 'fisher_test')
    }else{
      test_func <- c(test_func, 'chi_test')
    }
  }
  
  return(test_func)
}


get_stat_test <- function(df, group_col, value_col){
  
  method <- judge_test_name(df, group_col, value_col)
  
  p.value <- c()
  for(m in method){
    func_ <- test_dict[[m]]
    f <- as.formula(paste0(value_col, '~', group_col))
    test_res <- func_(f, df)
    
    p <- test_res$p.value
    p <- ifelse(p < 0.001, '<0.001', as.character(round(p,3)))
    p.value <- c(p.value, p)
  }
  res <- data.table(method, p.value)
  return(res)
}

