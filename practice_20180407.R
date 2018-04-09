aq <- read.csv("baq.csv")
meo <- read.csv("bmeo.csv")

library(dplyr)
library(ggplot2)

str(aq)
summary(aq)

faq <- aq %>% filter(stationId == "daxing_aq")
fmeo <- meo %>% filter(station_id == "daxing_meo")

all <- inner_join(faq, fmeo, by=c("utc_time"))

all %>% group_by(weather) %>% summarise(mean(PM2.5, na.rm = T))

all_month <- all %>% mutate(month = as.factor(substr(utc_time, 6,7))) %>% filter(wind_direction < 361)
str(all_month)
summary(all_month)


summary(aov(PM2.5~weather, data = all))
summary(aov(PM2.5~month, data = all_month))

bartlett.test(PM2.5~month, data = all_month)
#install.packages("agricolae")
#install.packages("laercio")
library(agricolae)
library(laercio)
aov_model <- aov(PM2.5~month, data = all_month)
summary(duncan.test(aov_model, "month", alpha=0.05))
LDuncan(aov_model, "month")

plot(all$PM2.5, all$PM10)
cor(all$PM2.5, all$PM10, use = "complete.obs")

str(all)
model <- lm (PM2.5 ~ NO2 + CO + SO2 + temperature + humidity + wind_direction + wind_speed + weather + month, data = all_month)

summary(model)
model_re <- step(model)
summary(model_re)

plot(model_re, which = 1)
plot(model_re, which = 2)

model_without_month <- lm (PM2.5 ~ NO2 + CO + SO2 + temperature + humidity + wind_direction + wind_speed + weather, data = all_month)
summary(model_without_month)
plot(model_without_month, which = 1)
plot(model_without_month, which = 2)

model_without_pollution <- lm (log(PM2.5) ~ temperature + humidity + wind_direction + wind_speed + weather, data = all_month)
summary(model_without_pollution)
plot(model_without_pollution)



model_log <- lm (log(PM2.5) ~ NO2 + CO + SO2 + temperature + humidity + wind_direction + wind_speed + weather + month, data = all_month)
summary(model_log)
plot(model_log)
plot(model_log, which = 1 )
plot(model_log, which = 2 )

cor(all_month[3:6], use = "complete.obs")

library("car")

completecase <- na.omit(all_month[3:8])
summary(completecase)


cor(completecase)


model_log <- lm (log(PM2.5) ~ pressure + humidity + wind_direction + wind_speed + weather, data = all_month)
summary(model_log)
plot(model_log, which = 1 )
plot(model_log, which = 2 )

## Decision Tree 01
library(rpart)

mp <- rpart(PM2.5 ~ pressure + humidity + wind_direction + wind_speed + weather, data = all_month)

pr2 <- predict(mp, all_month)
pr2

imputed_PM2 <- na.approx(all_month$PM2.5)
smape(imputed_PM2, pr2)


## Decision Tree 02 (temperature added)
mp2 <- rpart(PM2.5 ~ temperature + pressure + humidity + wind_direction + wind_speed + weather, data = all_month)

pr3 <- predict(mp2, all_month)
smape(imputed_PM2, pr3)


## Decision Tree 03 (Hour added)
library(tidyr)
all_month_time_separated <- all_month %>% mutate(time = utc_time) %>% separate(time, c("y","m","d","h"))
mp4 <- rpart(PM2.5 ~ temperature + pressure + humidity + wind_direction + wind_speed + weather + m + d + h, data = all_month_time_separated)
pr4 <- predict(mp4, all_month_time_separated)
smape(imputed_PM2, pr4)

## random forest 01 (Month Day Hour)
library(randomForest)
mp5 <- randomForest(PM2.5 ~ temperature + pressure + humidity + wind_direction + wind_speed + weather + m + d + h, data = na.omit(all_month_time_separated))
pr5 <- predict(mp5, all_month_time_separated)
smape(imputed_PM2, pr5)
summary(mp5)
mp5$importance
