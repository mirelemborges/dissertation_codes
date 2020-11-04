# Pacotes Necessarios
require(dplyr)
require(lubridate)
require(corrplot)

#--- FUNCAO ---#
var_lag_diff <- function(db, col_name_date, target_var, reference_value = 0.2)
{
  db <- db %>%
    arrange(get(col_name_date)) %>%
    mutate(
      lag1 = lag(get(target_var), 1),
      lag2 = lag(get(target_var), 2),
      lag3 = lag(get(target_var), 3),
      lag4 = lag(get(target_var), 4),
      lag5 = lag(get(target_var), 5),
      lag6 = lag(get(target_var), 6),
      lag7 = lag(get(target_var), 7),
      lag14 = lag(get(target_var), 14),
      lag30 = lag(get(target_var), 30),
      diff_lag7lag14 = lag7 - lag14,
      diff_lag1lag2 = lag1 - lag2,
      diff_lag1lag30 = lag1 - lag30,
      diff_lag1lag7 = lag1 - lag7
      )
  aux <- db %>%
    filter(!is.na(lag30)) %>%
    select(
      lag1,
      lag2,
      lag3,
      lag4,
      lag5,
      lag6,
      lag7,
      lag14,
      lag30,
      diff_lag7lag14,
      diff_lag1lag2,
      diff_lag1lag30,
      diff_lag1lag7,
      target_var)
  
  # lista de retornos da funcao
  list_return <- as.list(NULL)
  
  # retorno 1 
  ## plot das correlacoes dos lags e diffs e a variavel target
  correl <- cor(aux)
  list_return[[1]] <- correl
  
  # retorno 2 
  ## data frame com as variaves de lag ou diff que foram maiores que um valor estipulado
  
  correl <- as.data.frame(correl) 
  nome_linha <- row.names(correl)
  
  correl <- correl %>%
    mutate(linha = nome_linha) %>%
    filter(abs(get(target_var)) >= reference_value)
  
  selection <- c(correl$linha, 'linha')
  tirar <- colnames(correl)[!colnames(correl) %in% selection]
  
  list_return[[2]] <- db
  
  db <- db %>%
    select(-tirar)
  
  list_return[[3]] <- db
  
  return(list_return)
}

