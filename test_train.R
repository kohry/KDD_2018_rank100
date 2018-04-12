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

index <- sample(1:nrow(raw_w), 0.7 * nrow(raw_w), replace=F)
train_w <- raw_w[index,]
test_w <- raw_w[-index,]

# random forest 02 / 43.82
library(randomForest)
mp7 <- randomForest(PM2.5 ~ temperature + pressure + humidity + wind_direction + wind_speed + weather + h + day, data = na.omit(train_w))
pr7 <- predict(mp7, test_w)
imputed_PM25_a <- na.approx(test_w$PM2.5)
smape(imputed_PM25_a, pr7)
importance(mp7)

# XGBoost
onehot_h <- model.matrix(~h-1,raw_w )
onehot_weather <- model.matrix(~weather-1,raw_w )
onehot_day <- model.matrix(~day-1, raw_w)

raw_onehot <- cbind(raw_w, onehot_h, onehot_weather, onehot_day) %>% select(-stationId, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -station_id, -longitude, -latitude, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity))
str(raw_onehot)
raw_matrix <- data.matrix(raw_onehot)

data_train <- raw_matrix[index,-1]
data_train_label <- na.approx(raw_matrix[index,1])

data_test <- raw_matrix[-index,-1]
data_test_label <- na.approx(raw_matrix[-index,1])

dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)

xmodel2 <- xgboost(data = data_train, label = data_train_label, nround = 5, objective="reg:linear")
pred_xgba <- predict(xmodel2, data_test)
smape(data_test_label, pred_xgba)

