### dependency on eda1.R

str(j000_daxing)
str(b_meo_simple)
b_meo_simple %>% distinct(station_id) # daxing exists

daxing_simple <- b_meo_simple %>% filter(station_id == "daxing_meo")
weather_daxing_simple <- daxing_simple %>% select(utc_time, weather)


daxing_grid_weather <- inner_join(j000_daxing, weather_daxing_simple, by = "utc_time") 

str(daxing_grid_weather)

m_weather <- lm(PM2 ~ temperature + humidity + wind_speed.kph + h + weather, data = daxing_grid_weather)
summary(m_weather)

## We have to merge some weather indicator variable, so that we can get data from forecasting API
m_s_weather <- step(m_weather)
summary(m_s_weather)

par(mfrow = c(2, 2))
plot(m_s_weather)
par(mfrow = c(1, 1))

## coefficient weather is too high, and weather decide almost everything on the model.

plot(daxing_grid_weather$PM2.5)

pr00 <- predict(m_s_weather, daxing_grid_weather)
vi_jdx <- na.approx(daxing_grid_weather$PM2)


smape(vi_jdx, pr00)

#ggplot(daxing_grid_weather) + geom_line(aes(utc_time, PM2.5))




