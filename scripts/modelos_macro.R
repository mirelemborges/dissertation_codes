#--- Feature Engineering and Model Development ---#

# PREVER A QUANTIDADE GERAL POR DIA
require(dplyr)
require(bizdays)
require(purrr)
require(lubridate)
require(caret)
require(ggplot2)


# Loading Function (funcao que cria as variaveis de tempo)
source('functions/time_function.R')

# Loading Function (funcao que cria as variaveis de lag)
source('functions/time_lag_function.R')

#--- Reading Data
db <- readRDS('data/db_artigo.rds') %>%
  mutate(date_register = as.Date(date_register)) 


# Dividing datase (train and test)
# train (63.6%)
db_train <- db %>%
  filter(
    date_register >= '2018-01-01',
    date_register < '2019-01-01')

nrow(db_train)/nrow(db)

# test real (36.4%)
db_test_real <- db %>%
  filter(
    date_register >= '2019-01-01')

nrow(db_test_real)/nrow(db)

# teste para ser utilizado para criar as variaveis de lag
db_test <- db %>%
  filter(
    date_register >= '2018-12-31')



#--- Feature Engineering (demora para rodar)
## Criando as variaves de data (feriado, distancia de feriado, dia_util, etc)
db_train <- var_date(db = db_train, col_name_date = 'date_register', format_date = '%Y-%m-%d')


## Criando as variaves de lag e diff (lag2, lag7, diferenca entre lag2 e lag30, etc)
retorno <- var_lag_diff(db = db_train, col_name_date = 'date_register', target_var = 'quantity', reference_value = 0.5)


### analizado as correlacoes dos lags com a variavel resposta
corrplot::corrplot.mixed(retorno[[1]])


### sobreescrevendo o db_train pelo db com TODAS as variaveis MAIS as variaveis de lag
### DB TRAIN PRONTO PARA RODAR OS MODELOS
db_train <- retorno[[3]] %>%
  mutate(
    date_register = as.Date(date_register, format = '%Y-%m-%d'),
    bus_day = as.factor(bus_day),
    week = as.factor(week),
    is_bus = as.factor(is_bus),
    week_month = as.factor(week_month)) %>%
  select(-c(lag14, diff_lag1lag30)) %>%
  filter(!is.na(lag7))


#---- MODELS
# formula
form <- formula(quantity ~ week + is_bus + dist_holiday + week_month + lag7 + diff_lag1lag2)

# 5-fold cross validation
set.seed(1989)
tc <- caret::trainControl(method = "cv", number = 5)


# StepWise
set.seed(1989)
lm_step <- train(
  form,
  data = db_train, 
  method = "leapBackward",
  metric = "MAE",
  tuneGrid = data.frame(nvmax = 1:11),
  trControl = tc)

# metrics
lm_step$bestTune
summary(lm_step$finalModel)
coef(lm_step$finalModel, 6)
summary(lm_step)
lm_step$finalModel
lm_step$metric
lm_step$results

# FORMULA APOS SELECAO STEPWISE
form2 <- formula(quantity ~ week + is_bus + week_month + diff_lag1lag2)


# Regressao linear
set.seed(1989)
lm1 <- train(
  form2,
  data = db_train, 
  method = "lm",
  metric = "MAE",
  trControl = tc)

summary(lm1)

lm1$finalModel
lm1$results


# Random Forest 1
# tunning 500
set.seed(1989)
tg <- expand.grid(.mtry = seq(2, 10, by =2))

rf1 <- train(
  form2,
  data = db_train, 
  method = "rf",
  metric = "MAE",
  trControl = tc,
  tuneGrid = tg,
  ntree = 500)

summary(rf1)
rf1$finalModel
rf1$results


# Random Forest 2
# tunning 1000
set.seed(1989)
tg <- expand.grid(.mtry = seq(2, 10, by =2))

rf2 <- train(
  form2,
  data = db_train, 
  method = "rf",
  metric = "MAE",
  trControl = tc,
  tuneGrid = tg,
  ntree = 1000)

summary(rf2)
rf2$finalModel
rf2$results

# Neural Network
set.seed(1989)

nnetGrid <-  expand.grid(
  size = seq(from = 1, to = 10, by = 1),
  decay = seq(from = 0.1, to = 0.5, by = 0.1))

nn <- train(
  form2,
  data = db_train, 
  method = "nnet",
  metric = "MAE",
  trControl = tc,
  tuneGrid = nnetGrid,
  verbose = FALSE)

summary(nn)
nn$finalModel
nn$results

# Algorithm, ntree, mytry, size, decay

#- Metric Summary tabel
result_lm <- lm1$results %>%
  mutate(
    Algorithm = "Linear Regression",
    ntree = NA,
    mtry = NA,
    size = NA,
    decay = NA
  ) %>%
  select(Algorithm, ntree, mtry, size, decay, RMSE, Rsquared, MAE, RMSESD, RsquaredSD, MAESD)

result_rf1 <- rf1$results %>%
  mutate(
    Algorithm = "Random Forest",
    ntree = 500,
    size = NA,
    decay = NA
  ) 

result_rf2 <- rf2$results %>%
  mutate(
    Algorithm = "Random Forest",
    ntree = 1000,
    size = NA,
    decay = NA
  ) %>%
  select(Algorithm, ntree, mtry, size, decay, RMSE, Rsquared, MAE, RMSESD, RsquaredSD, MAESD)

result_nn <- nn$results %>%
  mutate(
    Algorithm = "Neural Network",
    ntree = NA,
    mtry = NA,
  ) %>%
  select(Algorithm, ntree, mtry, size, decay, RMSE, Rsquared, MAE, RMSESD, RsquaredSD, MAESD)


metric_table <- rbind(
  result_lm,
  result_rf1,
  result_rf2,
  result_nn
) 
write.table(metric_table,'results/metric_table.csv',row.names = FALSE)

#---- Aplicando o modelo na base de teste

## criando as variaveis na base de teste
db_test <- var_date(db = db_test, col_name_date = 'date_register', format_date = '%Y-%m-%d')


## Criando as variaves de lag e diff (lag2, lag7, diferenca entre lag2 e lag30, etc)
retorno <- var_lag_diff(db = db_test, col_name_date = 'date_register', target_var = 'quantity', reference_value = 0.02)


## sobreescrevendo o db de teste pelo db com TODAS as variaveis MAIS as variaveis de lag
db_test <- retorno[[3]] %>%
  mutate(
    date_register = as.Date(date_register, format = '%Y-%m-%d'),
    bus_day = as.factor(bus_day),
    week = as.factor(week),
    is_bus = as.factor(is_bus),
    week_month = as.factor(week_month)) %>%
  filter(
    !is.na(lag7),
    date_register >= '2019-01-01')
 


## aplicando os modelos na base de teste
db_test_scorado <- db_test %>%
  mutate(
    predicao_lm = predict(lm1, db_test),
    predicao_rf1 = predict(rf1, db_test),
    predicao_rf2 = predict(rf2, db_test),
    predicao_nn = predict(nn, db_test),
    )


# calcular RMSE para os modelos

rmse_lm = RMSE(pred = db_test_scorado$predicao_lm,obs = db_test_scorado$quantity)
rmse_rf1 = RMSE(pred = db_test_scorado$predicao_rf1,obs = db_test_scorado$quantity)
rmse_rf2 = RMSE(pred = db_test_scorado$predicao_rf2,obs = db_test_scorado$quantity)
rmse_nn = RMSE(pred = db_test_scorado$predicao_nn,obs = db_test_scorado$quantity)

rmse_lm = MAE(pred = db_test_scorado$predicao_lm,obs = db_test_scorado$quantity)
rmse_rf1 = MAE(pred = db_test_scorado$predicao_rf1,obs = db_test_scorado$quantity)
rmse_rf2 = MAE(pred = db_test_scorado$predicao_rf2,obs = db_test_scorado$quantity)
rmse_nn = MAE(pred = db_test_scorado$predicao_nn,obs = db_test_scorado$quantity)



# CONCLUSÂO: RAMDON FOREST COM MELHOR RMSE #


#--- RESULTS

# resultados na base de treino
db_train_scorado <- db_train %>%
  mutate(predicao = predict(rf1, db_train)) 

plot_train <- db_train_scorado %>%
  ggplot() +
  geom_line(aes(x = date_register, y = quantity, col = 'REAL')) +
  geom_line(aes(x= date_register, y = predicao, col = 'PREDICTION')) +
  labs(colour = "") +
  xlab('Register Date') +
  ylab('Quantity') +
  scale_x_date(date_labels = "%Y/%m")

plot_train

plot_train_zoom <- db_train_scorado %>%
  filter(date_register >= '2018-07-01') %>%
  ggplot() +
  geom_line(aes(x = date_register, y = quantity, col = 'REAL')) +
  geom_line(aes(x= date_register, y = predicao, col = 'PREDICTION')) +
  labs(colour = "") +
  xlab('Register Date') +
  ylab('Quantity') +
  scale_x_date(date_labels = "%Y/%m")

plot_train_zoom

# distribuicao da difrenca entre pretido e esperado
db_plot_dist <- db_train_scorado %>%
  mutate(
    `diferença` = quantity - predicao,
    qntd_entre_desvios = if_else(
      `diferença` < mean(`diferença`)+(sd(`diferença`)*2), 1, 0),
    qntd_alertas = if_else (quantity >= predicao + (sd(`diferença`)*2), 1, 0) 
  )

sum(db_plot_dist$qntd_alertas)
sum(db_plot_dist$qntd_entre_desvios)   


sd_diff_train <- sd(db_plot_dist$`diferença`)
nrow(db_plot_dist %>%filter(`diferença`>sd_diff_train))
nrow(db_plot_dist %>%filter(`diferença`>(sd_diff_train*2)))

nrow(db_plot_dist %>%filter(`diferença`>sd_diff_train))/nrow(db_plot_dist)*100
nrow(db_plot_dist %>%filter(`diferença`>(sd_diff_train*2)))/nrow(db_plot_dist)*100

db_plot_dist %>%
  ggplot(aes(x = `diferença`)) +
  geom_density() +
  geom_vline(aes(xintercept = sd_diff_train, color = "ALERT"), linetype = "dashed") +
  geom_vline(aes(xintercept = sd_diff_train *2, color = "ANOMALY"), linetype = "dashed") +
  scale_color_manual(name = "", values = c(ALERT = "#f3bf0d", ANOMALY ="red")) +
  ylab('Density') +
  xlab('Difference')



# resultados na base de teste

plot_test <- db_test_scorado %>%
  ggplot() +
  geom_line(aes(x = date_register, y = quantity, col = 'REAL')) +
  geom_line(aes(x= date_register, y = predicao_rf1, col = 'PREDICTION')) +
  labs(colour = "") +
  xlab('Register Date') +
  ylab('Quantity') +
  scale_x_date(date_labels = "%Y/%m")

plot_test



# detecção de anomalias
sd_pred <- sd(db_test_scorado$predicao_rf1)

db_test_scorado_anom <- db_test_scorado %>%
  mutate(
    alerta_warnin = if_else (quantity >= predicao_rf1 + sd_diff_train, 1, 0),
    alerta_real_warning = ifelse(alerta_warnin == 0, NA, alerta_warnin*quantity),
    alerta_dang = if_else (quantity >= predicao_rf1 + sd_diff_train*2, 1, 0),
    alerta_real_dang = ifelse(alerta_dang == 0, NA, alerta_dang*quantity)
  )

sum(db_test_scorado_anom$alerta_warnin)
sum(db_test_scorado_anom$alerta_dang)

plot_test_anom <- db_test_scorado_anom %>%
  ggplot() +
  geom_line(aes(x = date_register, y = quantity, color = 'REAL')) +
  geom_line(aes(x = date_register, y = predicao_rf1, col = 'PREDICTION')) +
  labs(colour = "") +
  geom_point(aes(x = date_register, y = alerta_real_warning, col = 'ALERT'), size = 3) +
  geom_point(aes(x = date_register, y = alerta_real_dang, col = 'ANOMALY'), size = 3) +
  xlab('Register Date') +
  ylab('Quantity') +
  scale_x_date(date_labels = "%Y/%m")
  

plot_test_anom




