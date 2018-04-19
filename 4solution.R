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
str(pollution)
str(bulk)

## 여기서부터 로직짜야함.

## station ID 는 뭐 해야햄.
bulk <- read.csv("bulk.csv") %>% filter(stationId == 1786458)


## 이것도 
df_aq_simple <- read.csv("beijing_17_18_aq.csv") %>% filter(stationId == "daxing_aq")


# joining bulk historical data with aq
raw <- inner_join(df_aq_simple, bulk, by=c("utc_time" = "date"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% filter(wind_direction < 361)
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

index <- sample(1:nrow(raw_w), 0.7 * nrow(raw_w), replace=F)
train_w <- raw_w[index,]
test_w <- raw_w[-index,]

# random forest 02 / 43.82

mp7 <- randomForest(PM2.5 ~ temperature + pressure + humidity + wind_direction + wind_speed + weather + h + m + day, data = na.omit(train_w))
pr7 <- predict(mp7, test_w)
imputed_PM25_a <- na.approx(test_w$PM2.5)
smape(imputed_PM25_a, pr7)
importance(mp7)

# XGBoost
onehot_h <- model.matrix(~h-1,raw_w )
onehot_m <- model.matrix(~m-1,raw_w)
onehot_weather <- model.matrix(~weather-1,raw_w )
onehot_day <- model.matrix(~day-1, raw_w)

#added
str(raw_w)

raw_onehot <- cbind(raw_w, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-stationId.x,-X,-stationId.y,-weather_code, -weather_desc, -weather_cloud, -weather_rain, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity))
str(raw_onehot)
raw_matrix <- data.matrix(raw_onehot)

data_train <- raw_matrix[index,-1]
data_train_label <- na.approx(raw_matrix[index,1])

data_test <- raw_matrix[-index,-1]
data_test_label <- na.approx(raw_matrix[-index,1])

dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)

xmodel2 <- xgboost(data = data_train, label = data_train_label, nround = 1500, objective="reg:linear")
pred_xgba <- predict(xmodel2, data_test)
smape(data_test_label, pred_xgba)



#####################

# load forecast data

# stretch to 48hour level forecast

# mapping all the station with forecast center

# predict PM2.5, PM10, O3 for each station




