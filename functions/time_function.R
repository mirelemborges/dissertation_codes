# Pacotes Necessarios
require(dplyr)
require(bizdays)
require(purrr)
require(lubridate)

#--- FUNCAO ---#
var_date <- function(db, col_name_date, format_date = "%d/%m/%Y", feriados = bizdays::holidaysANBIMA)
{
  
  # funcao auxiliar que calcula a distancia ate um feriado
  dist_holiday<-function(x, y = feriados)
  {
    # distancia entre datas
    menor_dist <- purrr::map2(x, y, difftime)
    db_min <- min(abs(unlist(menor_dist)))
    return(db_min)
  }
  
  db <- db %>%
    mutate(
      dt_ok = as.Date(get(col_name_date), format = format_date),
      first_day = ymd(format(dt_ok, "%Y-%m-01")),
      week = weekdays(dt_ok),
      is_bus = case_when(
        week == "sábado" | week == "domingo" | dt_ok %in% feriados ~ 0,
        TRUE ~ 1
      ),
      dist_holiday = unlist(purrr::map(.x = dt_ok, .f = dist_holiday)),
      week_month = stringi::stri_datetime_fields(get(col_name_date), tz = 'Etc/GMT-3')$WeekOfMonth
    )
  
  # creating the business day of the month
  temp <- db %>%
    filter(is_bus == 1) %>%
    mutate(
      var_temp = 1
    ) %>%
    group_by(first_day) %>%
    mutate(
      bus_day = cumsum(var_temp)) %>%
    ungroup() %>%
    select(dt_ok, bus_day)
  
  db <- db %>%
    left_join(temp, by = c("dt_ok" = "dt_ok")) %>%
    mutate(bus_day = if_else(is.na(bus_day),0,bus_day))
    
  
  return(db)
  
}


# depois de rodada a funcao voce ja pode usa-la
# a funcao precisa de 3 argumentos obrigatorios

# argumentos:

## db - eh a tua base de dados com a coluna de data (OBS a data tem que estar no formato dd/mm/aaaa, 
##       se nao estiver nesse formato tem que trocar o argumento "format_date")

## col_name_date - eh o nome da coluna do teu db que esta a tua data 
##                 (no exemplo abaixo a coluna se chama dt)

## feriados - eh um monte de datas que sao feriados esse banco ja vem pronto porem se voce quiser
##              acrescentar mais datas olhe o exemplo abaixo


# ----- EXEMPLO ----- #

# CRIANDO DB (VOCE VAI LER UM EXCEL)
# result <- c(500,345,450,200,300,444,534,673,263,374)
# dt <- c("01/05/2018","02/05/2018","03/05/2018","04/05/2018","05/05/2018","06/05/2018","07/05/2018","08/05/2018","09/05/2018","10/05/2018")
# dados <- tibble(result,dt) 

# CRIANDO A BASE DE FERIADOS
# para acrescentar feriados a lista de feriados basta colocar como no formato abaixo
# acrescentei o meu anivesario e o teu na base de feriados (depois eh so tirar)
# repare que o formato da data aqui eh outro ("aaaa-mm-dd")
# todos_feriados <- c(bizdays::holidaysANBIMA, as.Date("2019-05-08"), as.Date("2019-09-19"))


# PRONTO - AGORA EH SO RODAR O COMANDO ABAIXO
# db_com_variaveis <- var_date(db = dados, col_name_date = "dt", feriados = todos_feriados)


# agora voce ja tem a base pronta fazer o modelo essa base se chama "db_com_variaveis"



#----- MODELO -----#
# para fazer o modelo voce soh precisa dizer qual a variavel voce que prever e quais vao explicar
# assim:
# quero prever a variavel result
# e vou usar as variaveis week + eh_util + dist_feriados + semana_mes
# eh so colocar ali em formula (abaixo)
# 
# modelo <- lm(db_com_variaveis, formula = result ~ as.factor(week) + as.factor(eh_util) + dist_feriados + as.factor(semana_mes))
# 
# # equacao do modelo
# summary(modelo)
# 
# 
# # PREDICAO
# # criando uma base com a sua predicao.. pra ver se ta acertando
# pred <- predict(modelo, db_com_variaveis)
# 
# db_predict <- db_com_variaveis
# db_predict$pred <- pred
# 
# 
# # base com a predicao
# db_predict









