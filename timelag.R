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
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% filter(wind_direction < 361)
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

summary(raw_w)

#lag1 <- append(raw_w$PM2.5[2:nrow(raw_w)], c(NA))
#raw_lag <- cbind(raw_w, lag1)
#head(raw_lag)

raw_lag <- raw_w %>% mutate(lag1 = lag(raw_w$PM2.5,1))
raw_lag$lag1[1] = 109
head(raw_lag)


index <- sample(1:nrow(raw_lag), 0.7 * nrow(raw_lag), replace=F)
train_w <- raw_lag[index,]
test_w <- raw_lag[-index,]

# random forest 02 / 43.82
library(randomForest)
mp9 <- randomForest(PM2.5 ~ lag1 + temperature + pressure + humidity + wind_direction + wind_speed + weather + h + day, data = na.omit(train_w))
pr9 <- predict(mp9, test_w)
imputed_pr9 <- na.approx(pr9)
imputed_PM25_b <- na.approx(test_w$PM2.5)
smape(imputed_PM25_b, imputed_pr9)
importance(mp9)

# XGBoost
onehot_h <- model.matrix(~h-1,raw_lag )
onehot_weather <- model.matrix(~weather-1,raw_lag )
onehot_day <- model.matrix(~day-1, raw_lag)

raw_onehot <- cbind(raw_lag, onehot_h, onehot_weather, onehot_day) %>% select(-stationId, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -station_id, -longitude, -latitude, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity))
str(raw_onehot)
raw_matrix <- data.matrix(raw_onehot)

data_train <- raw_matrix[index,-1]
data_train_label <- na.approx(raw_matrix[index,1])

data_test <- raw_matrix[-index,-1]
data_test_label <- na.approx(raw_matrix[-index,1])

dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)

xmodel2 <- xgboost(data = data_train, label = data_train_label, nround = 1000, objective="reg:linear")
pred_xgba <- predict(xmodel2, data_test)
smape(data_test_label, pred_xgba)

