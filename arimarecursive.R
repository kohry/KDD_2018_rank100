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

##TIME, DAY 
raw <- inner_join(df_aq_simple, df_meo_simple, by=c("utc_time"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% mutate(wind_direction = ifelse(wind_direction > 361, 0, wind_direction))
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")

summary(raw_w)

raw_lag <- raw_w

raw_lag <- raw_w %>% mutate(lag1 = lag(PM2.5, 1))
raw_lag <- raw_lag[-1,]

# XGBoost
# 1) CATEGORICAL VARIABLE MUTATE
onehot_h <- model.matrix(~h-1,raw_lag )
onehot_weather <- model.matrix(~weather-1,raw_lag )
onehot_day <- model.matrix(~day-1, raw_lag)

# 2) FEATURE SELECT
raw_onehot <- cbind(raw_lag, onehot_h, onehot_weather, onehot_day) %>% select(-stationId, -utc_time, -PM10, -NO2, -CO, -O3, -SO2, -station_id, -longitude, -latitude, -weather, -y, -m, -d, -h, -day) %>% mutate(humidity = as.numeric(humidity)) 
colnames(raw_onehot)[which(names(raw_onehot) == "weatherSunny/clear")] <- "weatherSunny"

raw_onehot_lag <- raw_onehot %>% select(PM2.5, temperature, pressure, humidity, wind_direction, wind_speed, h00, h01, h02, h03, h04, h05, h06, h07, h08, h09, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21,h22,h23, weatherDust, weatherFog, weatherHaze, weatherRain, weatherSand, weatherSleet, weatherSnow, weatherSunny, daySun, dayMon, dayTue, dayWed, dayThu, dayFri, daySat, lag1)

raw_matrix <- data.matrix(raw_onehot_lag)




# 3) SAMPLING, LABEL NA IMPUTE
data_train <- raw_matrix[,-1]
data_train_label <- na.fill(raw_matrix[,1], "extend")

# 4) XGBOOST MATRIX CONVERSION
dtrain <- xgb.DMatrix(data = data_train, label = data_train_label)

result <- c()

# 5) MODELING
xmodel2 <- xgboost(data = data_train, label = data_train_label, nround = 16, objective="reg:linear")

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

rr <- matrix(nrow = 51, ncol = 8000)

for (start in 1: 2000) {
  
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

#write.csv(rr, "output5.csv")

#str(data_train_label)
#str(data_train)

#length(data_train_label)
#nrow(data_train)
#################################################################################################

