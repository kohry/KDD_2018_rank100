library(ggplot2)
library(dplyr)
library(xgboost)
library(lubridate)
library(tidyverse)
library(zoo)
library(randomForest)

smape <-function(yTest, yHat) {
  mean(abs((yTest-yHat)/(abs(yTest)+abs(yHat))))*200 
}

setwd("C://projectbuffer//contest//solution")

forecast <- read.csv("weather_forecast.csv")
current <- read.csv("weather_current.csv")
pollution <- read.csv("weather_pollution.csv")

str(forecast)
str(current)

## 여기서부터 로직짜야함.

## station ID 는 뭐 해야햄.
bulk <- read.csv("bulk.csv") %>% filter(stationId == 1786458)


## 이것도 
df_aq_simple <- read.csv("beijing_17_18_aq.csv") %>% filter(stationId == "daxing_aq")


# joining bulk historical data with aq
raw <- inner_join(df_aq_simple, bulk, by=c("utc_time" = "date"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% mutate(wind_direction = ifelse(wind_direction > 361, 0, wind_direction))
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

summary(raw_w)

raw_lag <- raw_w

raw_lag <- raw_w %>% mutate(lag1 = lag(PM2.5, 3))
raw_lag <- raw_lag[-1,]

# XGBoost
# 1) CATEGORICAL VARIABLE MUTATE
onehot_h <- model.matrix(~h-1,raw_lag )
onehot_weather <- model.matrix(~weather-1,raw_lag )
onehot_day <- model.matrix(~day-1, raw_lag)

# 2) FEATURE SELECT
raw_onehot <- cbind(raw_lag, onehot_h, onehot_weather, onehot_day) %>% select(-stationId.x,-X,-stationId.y,-weather_code, -weather_desc, -weather_cloud, -weather_rain,  -utc_time, -PM10, -NO2, -CO, -O3, -SO2,  -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity)) 
str(raw_onehot)
raw_onehot_lag <- raw_onehot %>% select(PM2.5, temperature, pressure, humidity, wind_direction, wind_speed, h00, h01, h02, h03, h04, h05, h06, h07, h08, h09, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21,h22,h23, weatherClear, weatherClouds, weatherDrizzle, weatherDust, weatherFog, weatherHaze, weatherMist, weatherRain, weatherSand, weatherSmoke, weatherSnow, weatherThunderstorm, daySun, dayMon, dayTue, dayWed, dayThu, dayFri, daySat, lag1)
raw_matrix <- data.matrix(raw_onehot_lag)




# 3) SAMPLING, LABEL NA IMPUTE
data_train <- raw_matrix[,-1]
data_train_label <- na.fill(raw_matrix[,1], "extend")

# 4) XGBOOST MATRIX CONVERSION
dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)

result <- c()

# 5) MODELING
xmodel2 <- xgboost(data = data_train, label = data_train_label, nround = 1500, objective="reg:linear")

data_train_lag_recursively_modified <- data_train

# 5.5) for recursive training, we have to get random variable from dataset.
recursivelyCalculate <- function (data, n) {
  
  if(n == 50) return(result)
  
  p <- predict(xmodel2, data[n:(n+1),])
  data[,"lag1"][n+1] <- p[1]
  
  ## for printing
  result[n] <- p[1]
  
  recursivelyCalculate(data,n+1)
  
}

#result <- recursivelyCalculate(data_train_lag_recursively_modified, 1)

rr <- matrix(nrow = 51, ncol = 2001)

for (start in 1: 1000) {
  
  if (start%%100 == 0) print(start)
  
  #print(paste("start : ", start))
  tt <- data_train_lag_recursively_modified[start : (start+51),]
  
  for (n in 1:50) {
    p <- predict(xmodel2, tt[n:(n+1),])
    tt[,"lag1"][n+1] <- p[1]
    result[n] <- p[1]
  }
  
  # 7) SMAPE MODIFY NEEDED
  for (n in 1:50) {
    m <- smape(data_train_label[(start+n)], result[n])
    rr[n, start] <- m
    
  }
  
}

mean(rr, na.rm = T)
