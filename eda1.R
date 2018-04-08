setwd("C://projectbuffer//contest")

#install.packages("tidyr")
#install.packages("DescTools")

library(dplyr)
library(ggplot2)
library(tidyr)
library(DescTools)
library(zoo)

smape <-function(yTest, yHat) {
  mean(abs((yTest-yHat)/(abs(yTest)+abs(yHat))))*200
  
}


b_aq_simple <- read.csv("beijing_17_18_aq.csv")
b_meo_simple <- read.csv("beijing_17_18_meo.csv")

b_aq_raw <- read.csv("beijing_historical_meo_grid.csv")

glimpse(b_aq_raw)

grid000 <- b_aq_raw %>% filter(stationName == "beijing_grid_000") %>% mutate(time = utc_time)


daxing_aq <- b_aq_simple %>% filter(stationId == "daxing_aq") %>% mutate(PM2 = PM2.5 + 0)

str(daxing_aq)

j000_daxing_t <- inner_join(daxing_aq, grid000, by = c("utc_time"))

# 이게 실제 train 데이터
j000_daxing <- j000_daxing_t %>% separate(time, c("y","m","d","h")) 

str(j000_daxing)

lm_000_daxing <- lm(PM2 ~ temperature + pressure + humidity + wind_direction + wind_speed.kph + m + h + d , data = j000_daxing)
summary(lm_000_daxing)


slm_000_daxing <- step(lm_000_daxing, direction="both")
summary(slm_000_daxing)
plot(slm_000_daxing, which=1)
plot(slm_000_daxing, which=2)

pr <- predict(slm_000_daxing, j000_daxing)

# imputing
v_imputed_pm2 <- na.approx(j000_daxing$PM2)

smape(v_imputed_pm2, pr)

str(j000_daxing)

