library(ggplot2)
library(dplyr)
library(xgboost)
library(lubridate)
library(tidyverse)
library(zoo)

smape <-function(yTest, yHat) {
  mean(abs((yTest-yHat)/(abs(yTest)+abs(yHat))))*200 
}

setwd("C://projectbuffer//contest")

df_aq_simple <- read.csv("beijing_17_18_aq.csv") %>% filter(stationId == "daxing_aq")
df_meo_simple <- read.csv("beijing_17_18_meo.csv") %>% filter(station_id == "daxing_meo")


raw <- inner_join(df_aq_simple, df_meo_simple, by=c("utc_time"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% mutate(wind_direction = ifelse(wind_direction > 361, 0, wind_direction))
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

summary(raw_w)

raw_lag <- raw_w

for(index in 1:100) {
  string <- paste("raw_lag"," <- raw_lag %>% mutate(lag", index ," = lag(raw_w$PM2.5,index))",sep="")
  eval(parse(text = string))
}


#raw_lag <- raw_w %>% mutate(lag1 = lag(raw_w$PM2.5,1))
raw_lag <- raw_lag[-c(1:101),]

#index <- sample(1:nrow(raw_lag), 0.7 * nrow(raw_lag), replace=F)
#train_w <- raw_lag[index,]
#test_w <- raw_lag[-index,]

#################################################################################################

# XGBoost
# 1) CATEGORICAL VARIABLE MUTATE
onehot_h <- model.matrix(~h-1,raw_lag )
onehot_weather <- model.matrix(~weather-1,raw_lag ) %>% mutate(weatherSunny = weatehrSunny/clear)
onehot_day <- model.matrix(~day-1, raw_lag)

# 2) FEATURE SELECT
raw_onehot <- cbind(raw_lag, onehot_h, onehot_weather, onehot_day) %>% select(-stationId, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -station_id, -longitude, -latitude, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity)) 
colnames(raw_onehot)[which(names(raw_onehot) == "weatherSunny/clear")] <- "weatherSunny"

for (index in 1:50) {
  string_lag_selection <- paste("raw_onehot_lag"," <- raw_onehot %>% select(PM2.5, temperature, pressure, humidity, wind_direction, wind_speed, h00, h01, h02, h03, h04, h05, h06, h07, h08, h09, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21,h22,h23, weatherDust, weatherFog, weatherHaze, weatherRain, weatherSand, weatherSleet, weatherSnow, weatherSunny, daySun, dayMon, dayTue, dayWed, dayThu, dayFri, daySat, lag",index,")", sep="")
  
  eval(parse(text = string_lag_selection))
  
  raw_matrix <- data.matrix(raw_onehot_lag)
  
  # 3) SAMPLING
  index <- sample(1:nrow(raw_lag), 0.7 * nrow(raw_lag), replace=F)
  
  data_train <- raw_matrix[index,-1]
  data_train_label <- na.approx(raw_matrix[index,1])
  data_test <- raw_matrix[-index,-1]
  data_test_label <- na.approx(raw_matrix[-index,1])
  
  # 4) XGBOOST MATRIX CONVERSION
  dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)
  
  # 5) MODELING
  xmodel2 <- xgboost(data = data_train, label = data_train_label, nround = 10, objective="reg:linear")
  
  # 6) PREDICT
  pred_xgba <- predict(xmodel2, data_test)
  
  # 7) SMAPE
  print(smape(data_test_label, pred_xgba))
}

#################################################################################################

