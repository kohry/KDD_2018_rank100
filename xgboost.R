library(ggplot2)
library(dplayr)
library(xgboost)
library(lubridate)
library(tidyverse)

df_aq_simple <- read.csv("beijing_17_18_aq.csv") %>% filter(stationId == "daxing_aq")
df_meo_simple <- read.csv("beijing_17_18_meo.csv") %>% filter(station_id == "daxing_meo")


raw <- inner_join(df_aq_simple, df_meo_simple, by=c("utc_time"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% filter(wind_direction < 361)
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

summary(raw_w)

# random forest 02
library(randomForest)
mp6 <- randomForest(PM2.5 ~ temperature + pressure + humidity + wind_direction + wind_speed + weather + h + day, data = na.omit(raw_w))
pr6 <- predict(mp6, raw_w)
imputed_PM25 <- na.approx(raw_w$PM2.5)
smape(imputed_PM25, pr6)
importance(mp6)

# XGBoost

onehot_h <- model.matrix(~h-1,raw_w )
onehot_weather <- model.matrix(~weather-1,raw_w )
onehot_day <- model.matrix(~day-1, raw_w)

raw_onehot <- cbind(raw_w, onehot_h, onehot_weather, onehot_day) %>% select(-stationId, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -station_id, -longitude, -latitude, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity))
str(raw_onehot)
raw_matrix <- data.matrix(raw_onehot)

data_train <- raw_matrix[,-1]
data_label <- na.approx(raw_matrix[,1])

dtrain <- xgb.DMatrix(data = data_train, label = data_label)

xmodel <- xgboost(data = data_train, label = data_label, nround = 500, objective="reg:linear")
pred_xgb <- predict(xmodel, data_train)
smape(imputed_PM25, pred_xgb)

