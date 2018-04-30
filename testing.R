
library(dplyr)
library(xgboost)
library(lubridate)
library(tidyverse)
library(zoo)
library(xts)
library(httr)

##문제가있다!
#원핫인코딩이 안된다는점과 forecast가져오는데 9시부터 가져와서 엉망진창이라는거!

setwd("C://projectbuffer//contest//solution")

current <- read.csv("weather_current.csv") # NOT NEEDED

forecast <- read.csv("weather_forecast.csv") # for get WEATHER FEATURE
pollution <- read.csv("weather_pollution.csv") # for get lagged POLLUTION

today <- Sys.Date()


# forecast preprocess
city_list <- c('dongsi_aq',	'tiantan_aq',	'guanyuan_aq',	'wanshouxigong_aq',	'aotizhongxin_aq',	'nongzhanguan_aq',	'wanliu_aq',	'beibuxinqu_aq',	'zhiwuyuan_aq',	'fengtaihuayuan_aq',	'yungang_aq',	'gucheng_aq',	'fangshan_aq',	'daxing_aq',	'yizhuang_aq',	'tongzhou_aq',	'shunyi_aq',	'pingchang_aq',	'mentougou_aq',	'pinggu_aq',	'huairou_aq',	'miyun_aq',	'yanqin_aq',	'dingling_aq',	'badaling_aq',	'miyunshuiku_aq',	'donggaocun_aq',	'yongledian_aq',	'yufa_aq',	'liulihe_aq',	'qianmen_aq',	'yongdingmennei_aq',	'xizhimenbei_aq',	'nansanhuan_aq',	'dongsihuan_aq','CD1'	,'BL0',	'GR4',	'MY7'	,'HV1',	'GN3',	'GR9',	'LW2',	'GN0',	'KF1',	'CD9',	'ST5',	'TH4')

city_list_china <- c('dongsi_aq',	'tiantan_aq',	'guanyuan_aq',	'wanshouxigong_aq',	'aotizhongxin_aq',	'nongzhanguan_aq',	'wanliu_aq',	'beibuxinqu_aq',	'zhiwuyuan_aq',	'fengtaihuayuan_aq',	'yungang_aq',	'gucheng_aq',	'fangshan_aq',	'daxing_aq',	'yizhuang_aq',	'tongzhou_aq',	'shunyi_aq',	'pingchang_aq',	'mentougou_aq',	'pinggu_aq',	'huairou_aq',	'miyun_aq',	'yanqin_aq',	'dingling_aq',	'badaling_aq',	'miyunshuiku_aq',	'donggaocun_aq',	'yongledian_aq',	'yufa_aq',	'liulihe_aq',	'qianmen_aq',	'yongdingmennei_aq',	'xizhimenbei_aq',	'nansanhuan_aq',	'dongsihuan_aq')
city_list_london <- c('CD1'	,'BL0',	'GR4',	'MY7'	,'HV1',	'GN3',	'GR9',	'LW2',	'GN0',	'KF1',	'CD9',	'ST5',	'TH4')


# forecast stretch

if (Sys.time() > paste(Sys.Date(),"09:00:00",sep=" ")) {
  today <- today + 1 #9시가 넘으면 그냥 하루를 넘긴다.
}

tt <- paste(today,"00:00:00",sep=" ")



date=zoo(rnorm(0), as.POSIXct(tt)+(0:47)*3600) #RRR date



timetable <- data.frame(rownames(data.frame(date)))
timetableeach <- merge(x = city_list, y = timetable, all = TRUE)
colnames(timetableeach) <- c("stationId","date")

merged_forecast <- merge(x = timetableeach, y = forecast, by = c("date","stationId"), all.x = TRUE) 

stretched_forecast <- merged_forecast[0,] # copy dataframe structure with empty data

for (inda in 1:length(city_list)) {
  
  ppp <- merged_forecast %>% filter(stationId == city_list[inda]) %>% arrange(date)
  ppp2 <- ppp
  ppp2$temperature <- ppp$temperature %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$pressure <- ppp$pressure %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$humidity <- ppp$humidity %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$weather_rain <- ppp$weather_rain %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$wind_speed <- ppp$wind_speed %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$wind_direction <- ppp$wind_direction %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$weather_cloud <- ppp$weather_cloud %>% na.approx(na.rm = F) %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F)
  ppp2$weather <- ppp$weather %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F) 
  ppp2$weather_desc <- ppp$weather_desc %>% na.locf(na.rm = F) %>% na.locf(fromLast = T, na.rm = F) 
  
  
  stretched_forecast <- rbind( stretched_forecast, ppp2)
  
}
# RR여기다가 NA값을 적당히 평균쳐서 채워줘야함

####################################### making lag
colnames(pollution) <- c("x", "id", "stationId", "date", "PM2.5","PM10","NO2","CO","O3","SO2")

date <- c()
PM2.5 <- c()
O3 <- c()
stationId <- c()
lag <- c()

#lags <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("date", "stationId", "PM2.5", "O3","lag"))
lags <- data.frame(date = character(), stationId = character(), PM2.5 = numeric(), PM10 = numeric(), O3 = numeric(), lag = numeric(), stringsAsFactors = F)

head(pollution %>% filter(stationId == "yongledian_aq") %>% arrange(date))

#각각의 최대 거시기를 뽑아줌. (각각 도시마다 최근 관측된 오염도가 다르니 이걸 탐지하고 lag를 뽑아줌.)
for (ind in 1:length(city_list)) {
  pol2 <- pollution %>% filter(stationId == city_list[ind]) %>% arrange(desc(date)) %>% select(stationId, date, PM2.5, PM10, O3)
  
  for(searchi in 1:6) { # 안찾아질수도 있으므로, 10번까지는 찾아본다.
    polhead <- pol2[searchi,]
    if (!is.na(polhead$PM2.5)) break
  }
  
  lag <- as.numeric(difftime(today, polhead[1,]$date, units ="hours"))
  lags[ind,] = c(as.character(polhead$date), polhead$stationId, polhead$PM2.5, polhead$PM10, polhead$O3, lag)
  
}

# join all together
purified_lags <- lags %>% select(stationId, PM2.5, PM10, O3, lag) %>% mutate(PM2.5 = as.numeric(PM2.5), O3 = as.numeric(PM10), lag = as.numeric(lag))
purified_forecast <- stretched_forecast %>% mutate(stationIdNum = as.numeric(stationId))
joined_data <- merge(x = purified_forecast, y = purified_lags, by.x = "stationIdNum", by.y= "stationId", all.x = TRUE) 

# predict with prediction model.

submission <- read.csv("submission.csv")
sub <- submission %>% separate("test_id", into = c("stationId", "day"), sep = "#")


##for template for input data
bulk_china <- read.csv("bulk.csv") %>% filter(stationId == 1786458)
df_aq_simple <- read.csv("beijing_17_18_aq.csv")
raw <- inner_join(df_aq_simple, bulk_china, by=c("utc_time" = "date"))
raw_time <- raw %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h")) %>% filter(wind_direction < 361)
raw_w <- raw_time %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h))
raw_w$day <- wday(raw_w$utc_time, label = T, locale="english")
raw_w$lag <- lag(raw_w$PM2.5,1)
template <- raw_w[0,]


weatherlevel <- levels(template$weather)

## CHINA PM2.5
for (icity in 1:length(city_list_china)) {
  
  print(city_list_china[icity])
  
  d1 <- joined_data %>% mutate(time = date) %>% separate(time, c("y","m","d","h")) %>% mutate(utc_time = date) %>% filter(stationIdNum == icity)
  d2 <- d1 %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h)) %>% arrange(date)
  d2$day <- wday(d2$date, label = T, locale="english")
  d3 <- d2 %>% mutate(lag = PM2.5) %>% select(date, temperature, pressure, humidity, weather, wind_speed, wind_direction, m, h, day, lag) 
  
  levels(d3$weather) <- levels(template$weather)
  levels(d3$m) <- levels(template$m)
  levels(d3$h) <- levels(template$h)
  levels(d3$day) <- levels(template$day)
  
  # XGBoost
  onehot_h <- model.matrix(~h-1,d3)
  onehot_m <- model.matrix(~m-1,d3)
  onehot_day <- model.matrix(~day-1, d3)
  
  ###replace
  onehot_weather <- model.matrix(~weather-1,d3 )
  weathers <- d2$weather
  
  for(aax in 1:48) {
    iix <- which(weatherlevel == weathers[aax])
    onehot_weather[aax,] = 0
    onehot_weather[aax, iix] = 1
  }
  
  #added
  d4_onehot <- cbind(d3, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-date, -weather, -h, -m, -day) %>% mutate(humidity = as.numeric(humidity))
  
  d4_onehot$m01 <- 0 ##RR
  d4_onehot$m04 <- 1 ##RR
  
  d5_matrix <- data.matrix(d4_onehot)
  
  for(iter in 1:48) {
    
    lagindex <- (iter + as.numeric(d2$lag[1]) - 1)
    str <- ifelse(is.na(lagindex) | lagindex > 55, "xgbII_model_china_general", paste("xgbII_model_china",icity,lagindex, sep="_")) 
    
    if (str == "xgbII_model_china_general") {
      currentCursor = rbind(d5_matrix[iter,-6],d5_matrix[iter,-6])
    } else {
      currentCursor = rbind(d5_matrix[iter,],d5_matrix[iter,])
    }
    
    model <- xgb.load(str)
    result <- predict(model, currentCursor)
    printindex <- (48 * (icity - 1) + iter)
    
    sub[printindex, c("PM2.5")] = ifelse(result[1] < 0, 1, result[1])
    
  }
  
}



## LONDON PM2.5
for (icity in 36:48) {
  
  print(city_list[icity])
  
  d1 <- joined_data %>% mutate(time = date) %>% separate(time, c("y","m","d","h")) %>% mutate(utc_time = date) %>% filter(stationIdNum == icity)
  d2 <- d1 %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h)) %>% arrange(date)
  d2$day <- wday(d2$date, label = T, locale="english")
  d3 <- d2 %>% mutate(lag = PM2.5) %>% select(date, temperature, pressure, humidity, weather, wind_speed, wind_direction, m, h, day, lag)
  
  levels(d3$weather) <- levels(template$weather)
  levels(d3$m) <- levels(template$m)
  levels(d3$h) <- levels(template$h)
  levels(d3$day) <- levels(template$day)
  
  # XGBoost
  onehot_h <- model.matrix(~h-1,d3)
  onehot_m <- model.matrix(~m-1,d3)
  onehot_weather <- model.matrix(~weather-1,d3 )
  onehot_day <- model.matrix(~day-1, d3)
  
  for(aax in 1:48) {
    iix <- which(weatherlevel == weathers[aax])
    onehot_weather[aax,] = 0
    onehot_weather[aax, iix] = 1
  }
  
  #added
  d4_onehot <- cbind(d3, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-date, -weather, -h, -m, -day) %>% mutate(humidity = as.numeric(humidity))
  
  d4_onehot$m01 <- 0 ##RR
  d4_onehot$m04 <- 1 ##RR
  
  d5_matrix <- data.matrix(d4_onehot)
  
  
  
  for(iter in 1:48) {
    
    lagindex <- (iter + as.numeric(d2$lag[1]) - 1)
    #str <- "xgbII_model_london_general"  ##RRRRRR ifchanged,
    ifelse(is.na(lagindex) | lagindex > 55, "xgbII_model_london_general", paste("xgbII_model_london",icity,lagindex, sep="_")) 
    
    if (str == "xgbII_model_london_general") {
      currentCursor = rbind(d5_matrix[iter,-6],d5_matrix[iter,-6])
    } else {
      currentCursor = rbind(d5_matrix[iter,],d5_matrix[iter,])
    }
    
    model <- xgb.load(str)
    result <- predict(model, currentCursor)
    printindex <- (48 * (icity - 1) + iter)
    
    sub[printindex, c("PM2.5")] = ifelse(result[1] < 0, 1, result[1])
    
  }
  
}


## CHINA PM10
for (icity in 1:length(city_list_china)) {
  
  print(city_list_china[icity])
  
  d1 <- joined_data %>% mutate(time = date) %>% separate(time, c("y","m","d","h")) %>% mutate(utc_time = date) %>% filter(stationIdNum == icity)
  d2 <- d1 %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h)) %>% arrange(date)
  d2$day <- wday(d2$date, label = T, locale="english")
  d3 <- d2 %>% mutate(lag = PM2.5) %>% select(date, temperature, pressure, humidity, weather, wind_speed, wind_direction, m, h, day, lag)
  
  levels(d3$weather) <- levels(template$weather)
  levels(d3$m) <- levels(template$m)
  levels(d3$h) <- levels(template$h)
  levels(d3$day) <- levels(template$day)
  
  # XGBoost
  onehot_h <- model.matrix(~h-1,d3)
  onehot_m <- model.matrix(~m-1,d3)
  onehot_weather <- model.matrix(~weather-1,d3 )
  onehot_day <- model.matrix(~day-1, d3)
  
  pia = (48 * (icity - 1)) +1
  
  for(aax in 1:48) {
    iix <- which(weatherlevel == weathers[aax])
    onehot_weather[aax,] = 0
    onehot_weather[aax, iix] = 1
  }
  
  #added
  d4_onehot <- cbind(d3, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-date, -weather, -h, -m, -day) %>% mutate(humidity = as.numeric(humidity))
  
  d4_onehot$m01 <- 0 ##RR
  d4_onehot$m04 <- 1 ##RR
  
  d5_matrix <- data.matrix(cbind(sub[pia:(pia+47),c("PM2.5")], d4_onehot[,-6]))
  
  colnames(d5_matrix)[1] <- "PM2.5"
  
  for(iter in 1:48) {
    
    str <- "xgbII_model_china_general_pm10"
    
    currentCursor <- rbind(d5_matrix[iter,],d5_matrix[iter,])
    
    model <- xgb.load(str)
    result <- predict(model, currentCursor)
    printindex <- (48 * (icity - 1) + iter)
    
    sub[printindex, c("PM10")] = ifelse(result[1] < 0, 1, result[1])
    
  }
  
}



## LONDON PM10
for (icity in 36:48) {
  
  print(city_list[icity])
  
  d1 <- joined_data %>% mutate(time = date) %>% separate(time, c("y","m","d","h")) %>% mutate(utc_time = date) %>% filter(stationIdNum == icity)
  d2 <- d1 %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h)) %>% arrange(date)
  d2$day <- wday(d2$date, label = T, locale="english")
  d3 <- d2 %>% mutate(lag = PM2.5) %>% select(date, temperature, pressure, humidity, weather, wind_speed, wind_direction, m, h, day, lag)
  
  levels(d3$weather) <- levels(template$weather)
  levels(d3$m) <- levels(template$m)
  levels(d3$h) <- levels(template$h)
  levels(d3$day) <- levels(template$day)
  
  # XGBoost
  onehot_h <- model.matrix(~h-1,d3)
  onehot_m <- model.matrix(~m-1,d3)
  onehot_weather <- model.matrix(~weather-1,d3 )
  onehot_day <- model.matrix(~day-1, d3)
  
  pia = (48 * (icity - 1)) +1
  
  for(aax in 1:48) {
    iix <- which(weatherlevel == weathers[aax])
    onehot_weather[aax,] = 0
    onehot_weather[aax, iix] = 1
  }
  
  #added
  d4_onehot <- cbind(d3, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-date, -weather, -h, -m, -day) %>% mutate(humidity = as.numeric(humidity))
  
  d4_onehot$m01 <- 0 ##RR
  d4_onehot$m04 <- 1 ##RR
  
  
  d5_matrix <- data.matrix(cbind(sub[pia:(pia+47),c("PM2.5")], d4_onehot[,-6]))
  
  colnames(d5_matrix)[1] <- "PM2.5"
  
  for(iter in 1:48) {
    
    str <- "xgbII_model_london_general_pm10"
    
    currentCursor <- rbind(d5_matrix[iter,],d5_matrix[iter,])
    
    model <- xgb.load(str)
    result <- predict(model, currentCursor)
    printindex <- (48 * (icity - 1) + iter)
    
    sub[printindex, c("PM10")] = ifelse(result[1] < 0, 1, result[1])
    
  }
  
}


# predict O3 level for china
for (icity in 1:length(city_list_china)) {
  
  print(city_list_china[icity])
  
  d1 <- joined_data %>% mutate(time = date) %>% separate(time, c("y","m","d","h")) %>% mutate(utc_time = date) %>% filter(stationIdNum == icity)
  d2 <- d1 %>% mutate(y = as.factor(y), m = as.factor(m), d = as.factor(d), h = as.factor(h)) %>% arrange(date)
  d2$day <- wday(d2$date, label = T, locale="english")
  d3 <- d2 %>% mutate(lag = PM2.5) %>% select(date, temperature, pressure, humidity, weather, wind_speed, wind_direction, m, h, day, lag)
  
  levels(d3$weather) <- levels(template$weather)
  levels(d3$m) <- levels(template$m)
  levels(d3$h) <- levels(template$h)
  levels(d3$day) <- levels(template$day)
  
  # XGBoost
  onehot_h <- model.matrix(~h-1,d3)
  onehot_m <- model.matrix(~m-1,d3)
  onehot_weather <- model.matrix(~weather-1,d3 )
  onehot_day <- model.matrix(~day-1, d3)
  
  for(aax in 1:48) {
    iix <- which(weatherlevel == weathers[aax])
    onehot_weather[aax,] = 0
    onehot_weather[aax, iix] = 1
  }
  
  #added
  d4_onehot <- cbind(d3, onehot_h,onehot_m, onehot_weather, onehot_day) %>% select(-date, -weather, -h, -m, -day) %>% mutate(humidity = as.numeric(humidity))
  d4_onehot$m01 <- 0 ##RR
  d4_onehot$m04 <- 1 ##RR
  
  
  d5_matrix <- data.matrix(d4_onehot)
  
  for(iter in 1:48) {
    
    lagindex <- (iter + as.numeric(d2$lag[1]) - 1)
    str <- ifelse(is.na(lagindex) | lagindex > 55, "xgbII_model_O3_general_without_pm2", paste("xgbII_model_china_O3",icity,lagindex, sep="_")) 
    
    if (str == "xgbII_model_O3_general_without_pm2") {
      currentCursor = rbind(d5_matrix[iter,-6],d5_matrix[iter,-6])
    } else {
      currentCursor = rbind(d5_matrix[iter,],d5_matrix[iter,])
    }
    
    model <- xgb.load(str)
    result <- predict(model, currentCursor)
    printindex <- (48 * (icity - 1) + iter)
    
    sub[printindex, c("O3")] = ifelse(result[1] < 0, 0, result[1])
    
  }
  
}


## FILE SAVE
sub_final <- (sub %>% mutate(test_id = paste(stationId, day, sep="#")))[,c(6,3,4,5)]
filename <- paste("zfinal_sumission",as.numeric(Sys.time()),".csv")
write.csv(sub_final, filename, row.names = F)
write.csv(sub_final, "current_submission.csv", row.names = F)

