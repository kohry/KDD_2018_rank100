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

## station ID 는 뭐 해야햄.
bulk_china <- read.csv("bulk.csv") %>% filter(stationId == 1786458)
city_list <- c('dongsi_aq',	'tiantan_aq',	'guanyuan_aq',	'wanshouxigong_aq',	'aotizhongxin_aq',	'nongzhanguan_aq',	'wanliu_aq',	'beibuxinqu_aq',	'zhiwuyuan_aq',	'fengtaihuayuan_aq',	'yungang_aq',	'gucheng_aq',	'fangshan_aq',	'daxing_aq',	'yizhuang_aq',	'tongzhou_aq',	'shunyi_aq',	'pingchang_aq',	'mentougou_aq',	'pinggu_aq',	'huairou_aq',	'miyun_aq',	'yanqin_aq',	'dingling_aq',	'badaling_aq',	'miyunshuiku_aq',	'donggaocun_aq',	'yongledian_aq',	'yufa_aq',	'liulihe_aq',	'qianmen_aq',	'yongdingmennei_aq',	'xizhimenbei_aq',	'nansanhuan_aq',	'dongsihuan_aq')

###############################################

smapes <- matrix(nrow = 40, ncol = 55)

## 이부분 루프돌기
df_aq_simple <- read.csv("beijing_17_18_aq.csv")

start_time <- Sys.time()

# joining bulk historical data with aq
raw <- inner_join(df_aq_simple, bulk_china, by=c("utc_time" = "date"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% filter(wind_direction < 361)
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

index <- sample(1:nrow(raw_w), 0.9 * nrow(raw_w), replace=F)
train_w <- raw_w[index,]
test_w <- raw_w[-index,]

# XGBoost
onehot_h <- model.matrix(~h-1,raw_w )
onehot_m <- model.matrix(~m-1,raw_w)
onehot_weather <- model.matrix(~weather-1,raw_w )
onehot_day <- model.matrix(~day-1, raw_w)

#added
raw_onehot <- cbind(raw_w, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-stationId.x,-X,-stationId.y,-weather_code, -weather_desc, -weather_cloud, -weather_rain, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity))

raw_matrix <- data.matrix(raw_onehot)

data_train <- raw_matrix[index,-1]
data_train_label <- na.approx(raw_matrix[index,1])

data_test <- raw_matrix[-index,-1]
data_test_label <- raw_matrix[-index,1] %>% na.locf(na.rm = F) %>% na.locf(na.rm = F, fromLast = T) %>% na.approx(na.rm = F)

dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)

xmodel <- xgboost(data = data_train, label = data_train_label, nround = 1500, objective="reg:linear")
pred_xgba <- predict(xmodel, data_test)

xgb.save(xmodel, "xgbII_model_china_general")
smape(data_test_label, pred_xgba)


