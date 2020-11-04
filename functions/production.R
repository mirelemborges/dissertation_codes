# Pacotes Necessarios
require(dplyr)
require(lubridate)
require(corrplot)


#--- FUNCAO ---#
preditic_mes <- function(db, col_name_date, target_var, modelo, format_date = '%Y-%m-%d')
{
  db <- db %>%
    select(col_name_date, target_var)
  
  # adicionando um mes para frente
  max_aux <- db %>%
    summarise(maximo = max(get(col_name_date)))
  
  mes_futuro <- seq.Date(max_aux$maximo+1, seq(max_aux$maximo+1, length=2, by = 'month')[2]-1, by = 'day')
  aux <- rep(0, length(mes_futuro))
  
  db_aux <- data.frame(mes_futuro, aux) 
  colnames(db_aux)[1] <- col_name_date
  colnames(db_aux)[2] <- target_var
  
  
  # acrescentando o mes futuro na base que tem o mes presente
  db <- rbind(db, db_aux)
  
  
  # carregando funcao var_date()
  source('functions/time_function.R')
  
  
  ## Criando as variaves de data (feriado, distancia de feriado, eh_util, etc)
  db <- var_date(db = db, col_name_date = col_name_date, format_date = format_date)
  
  
  # Criando as variaves de lag
  db <- db %>%
    arrange(get(col_name_date)) %>%
    mutate(
      lag1 = lag(get(target_var), 1),
      lag2 = lag(get(target_var), 2),
      lag3 = lag(get(target_var), 3),
      lag4 = lag(get(target_var), 4),
      lag5 = lag(get(target_var), 5),
      lag6 = lag(get(target_var), 5),
      lag7 = lag(get(target_var), 7),
      lag14 = lag(get(target_var), 14),
      lag30 = lag(get(target_var), 30),
      dif_lag7lag14 = lag7 - lag14,
      dif_lag1lag2 = lag1 - lag2,
      dif_lag1lag30 = lag1 - lag30,
      dif_lag1lag7 = lag1 - lag7
    ) %>%
    filter(!is.na(lag30))
  
  
  # PREDICTING
  db_scorado <- db %>%
    mutate(predicao = predict(modelo, db)) %>%
    select(col_name_date, 'predicao')
  
  # retorno da funcao
  return(db_scorado)
}

#-- INPUTS DA FUNCAO --#
# db_ultimo_mes <- db %>%
#   filter(
#     data_realizacao >= '2019-07-01',
#     data_realizacao <= '2019-07-31')
#
# col_name_date <- 'data_realizacao'
# 
# target_var <- 'qntd'
# 
# modelo <- lm_step



