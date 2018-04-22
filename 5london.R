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
bulk_england <- read.csv("bulk.csv") %>% filter(stationId == 2643743)
city_list <- c('CD1'	,'BL0',	'GR4',	'MY7'	,'HV1',	'GN3',	'GR9',	'LW2',	'GN0',	'KF1',	'CD9',	'ST5',	'TH4')

###############################################

smapes <- matrix(nrow = 20, ncol = 55)

iii = 1
iilag = 1

while (iii <= length(city_list)) {
  
  ## 이부분 루프돌기
  df_aq_simple <- read.csv("london_aq.csv") %>% filter(stationId == city_list[iii])
  iilag = 1
  
  while (iilag <= 55) {
    
    # joining bulk historical data with aq
    raw <- inner_join(df_aq_simple, bulk_england, by=c("utc_time" = "date"))
    raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% filter(wind_direction < 361)
    raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
    raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")
    
    raw_w$lag <- lag(raw_w$PM2.5,iilag)
    raw_w <- raw_w %>% drop_na(PM2.5, lag)
    
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
    data_train_label <- raw_matrix[index,1]
    
    data_test <- raw_matrix[-index,-1]
    data_test_label <- raw_matrix[-index,1]
    
    tryCatch({
      
      dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)
      
      xmodel <- xgboost(data = data_train, label = data_train_label, nround = 1500, objective="reg:linear", verbose = 0)
      pred_xgba <- predict(xmodel, data_test)
      
      xgb.save(xmodel, paste("xgbII_model_london",iii,iilag,sep="_"))
      smapes[iii,iilag] <- smape(data_test_label, pred_xgba)
      print(smapes[iii,iilag])
      
      print( Sys.time() )
      
      iilag = iilag + 1
      
    }, error = function(cond) {message(cond)})
    
  }
  
  iii = iii + 1
  write.csv(smapes, "xgbII_model_london_smapes.csv") ##RRRR
  
}


 #####################

# load forecast data

# stretch to 48hour level forecast

# mapping all the station with forecast center

# predict PM2.5, PM10, O3 for each station






