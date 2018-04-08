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

summary(aov(PM2.5~weather, data = all))
summary(aov(PM2.5~month, data = all_month))

bartlett.test(PM2.5~month, data = all_month)
#install.packages("agricolae")
install.packages("laercio")
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


library(rpart)

mp <- rpart(PM2.5 ~ pressure + humidity + wind_direction + wind_speed + weather, data = all_month)
summary(mp)
plot(mp)

predict(mp, all_month)
