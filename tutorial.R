# install.packages(c("dplyr", "ggplot2", "olsrr", "PerformanceAnalytics", "libridate", "forecast", "car", "tidyverse"))
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)
library(gridExtra)
library(car)
library(tidyverse)

ghg <- read.csv("/cloud/project/Deemer_GHG_Data.csv")

# Log variable transformation. For some variable, we add 1 b/c log can't take value of 0
ghg$log.ch4 <- log(ghg$ch4+1) 
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

# Constructing Binary Variables
unique(ghg$Region)
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# Running a multiple regression
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg)
summary(mod.full)

# checking assumptions
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

chart.Correlation(reg.data, histogram=TRUE, pch=19)


# Model Selection
full.step <- ols_step_forward_aic(mod.full)
full.step # view table
full.step$model #check full model
plot(full.step)

# Predictions
# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")
# look at prediction with 95% confidence interval of the mean
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")

###### Chapter 9: An introduction to time series
ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")
unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
  
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# Almond ET time series
almond_ts <- ts(almond$ET.in, 
                start = c(2016,1), #start year 2016, month 1
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts)) 

almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmondF <- data.frame(newAlmond)
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

# Homework Question 1
ghg$transformed_CO2 <- 1/(ghg$co2 + 1000)

# Transforming variable and analyzing bivariate plots
ghg$log.mean.depth <- log(ghg$mean.depth)
ghg$log.runoff <-log(ghg$runoff)
ghg$log.chlorophyll.a <- log(ghg$chlorophyll.a)
ghg$log.surface.area <- log(ghg$surface.area)

plot1 <- ggplot(ghg, aes(x = airTemp, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot2 <- ggplot(ghg, aes(x = mean.depth, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot3 <- ggplot(ghg, aes(x = log.mean.depth, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot4 <- ggplot(ghg, aes(x = runoff, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot5 <- ggplot(ghg, aes(x = log.runoff, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot6<- ggplot(ghg, aes(x = chlorophyll.a, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot7<- ggplot(ghg, aes(x = log.chlorophyll.a, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot8<- ggplot(ghg, aes(x = surface.area, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")
plot9<- ggplot(ghg, aes(x = log.surface.area, y = transformed_CO2)) + geom_point() + geom_smooth(method = "lm")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol = 5)

# multiple regression
model <- lm(transformed_CO2 ~ airTemp + log.age + log.mean.depth 
            + log.DIP + log.precip + log.ch4 + log.chlorophyll.a + 
              log.surface.area + runoff, data = ghg)
model_summary <- summary(model)
sample_size = nrow(ghg) - 245

# Assumption testing
res.full <- rstandard(model)
fit.full <- fitted.values(model)
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# Q-Q plot for normality
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# shapiro.test
shapiro.test(res.full)

# Check multicollinearity
multi.reg.data <- data.frame(ghg$airTemp, ghg$log.age, ghg$log.mean.depth, ghg$log.DIP, 
                             ghg$log.precip, ghg$log.ch4, ghg$log.chlorophyll.a, 
                             ghg$log.surface.area, ghg$runoff)

chart.Correlation(multi.reg.data, histogram=TRUE, pch=19)

# Model Selection
full.step <- ols_step_forward_aic(model)
plot(full.step)

# Question 2: Decompose the evapotranspiration time series for selected crops
crops <- c("Almonds", "Pistachios", "Fallow/Idle Cropland", "Corn", "Grapes (Table/Raisin)")
decompose_crop <- function(crop_name) {
  crop_data <- ETdat %>%
    filter(crop == crop_name) %>%
    group_by(date) %>%
    summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))  # Average ET by month
  
  crop_ts <- ts(crop_data$ET.in, start = c(2016, 1), frequency = 12)
  decomposition <- decompose(crop_ts)
  plot(decomposition)
  title(main = paste("ET Decomposition for", crop_name))}

decompose_crop("Almonds")
decompose_crop("Pistachios")
decompose_crop("Fallow/Idle Cropland")
decompose_crop("Corn")
decompose_crop("Grapes (Table/Raisin)")

# Question 3: Generate Pistachios and Fallow fields autoregressive model
pistachio_data <- ETdat %>% 
  filter(crop == "Pistachios") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

fallow_data <- ETdat %>% 
  filter(crop == "Fallow/Idle Cropland") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

pistachio_ts <- ts(pistachio_data$ET.in, start = c(2016, 1), frequency = 12)
fallow_ts <- ts(fallow_data$ET.in, start = c(2016, 1), frequency = 12)

# Calculate AR Fit and Build AR Model
pacf_2.plot <- pacf(na.omit(pistachio_ts))
pacf_3.plot <- pacf(na.omit(fallow_ts))

pistachio_y <- na.omit(pistachio_ts)
pistachio_model3 <- arima(pistachio_y, order = c(3, 0, 0))
pistachio_model4 <- arima(pistachio_y, order = c(4, 0, 0))

pistachio_fit3 <- pistachio_y - residuals(pistachio_model3) 
pistachio_fit4 <- pistachio_y - residuals(pistachio_model4) 

plot(pistachio_y)
points(pistachio_fit3, type = "l", col = "tomato3", lty = 2, lwd=2)
points(pistachio_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR3","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")


fallow_y <- na.omit(fallow_ts)
fallow_model3 <- arima(fallow_y, order = c(3, 0, 0))
fallow_model4 <- arima(fallow_y, order = c(4, 0, 0))

fallow_fit3 <- fallow_y - residuals(fallow_model3) 
fallow_fit4 <- fallow_y - residuals(fallow_model4) 

plot(fallow_y)
points(fallow_fit3, type = "l", col = "tomato3", lty = 2, lwd=2)
points(fallow_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR3","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

# Forecast for 24 months
pistachio_forecast <- forecast(pistachio_model4)
fallow_forecast <- forecast(fallow_model4)

# Convert forecasts to data frames for plotting
newPistachioF <- data.frame(pistachio_forecast)
newFallowF <- data.frame(fallow_forecast)

# Set up dates for forecasted periods
years <- c(rep(2021, 4), rep(2022, 12), rep(2023, 8))
months <- c(seq(9, 12), seq(1, 12), seq(1, 8))

newPistachioF$dateF <- ymd(paste(years, "/", months, "/", 1))
newFallowF$dateF <- ymd(paste(years, "/", months, "/", 1))

pistachio_ET <- ggplot() +
  geom_line(data = pistachio_data, aes(x = ymd(date), y = ET.in), color = "blue") +
  xlim(ymd(pistachio_data$date[1]), newPistachioF$dateF[24]) +
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast), color = "red") +
  geom_ribbon(data = newPistachioF, aes(x = dateF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(title = "Pistachio Field ET Forecast", x = "Year", y = "Evapotranspiration (inches)")

Fallow_ET <- ggplot() +
  geom_line(data = fallow_data, aes(x = ymd(date), y = ET.in), color = "blue") +
  xlim(ymd(fallow_data$date[1]), newFallowF$dateF[24]) + 
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast), color = "red") +  
  geom_ribbon(data = newFallowF, aes(x = dateF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(title = "Fallow/Idle Field ET Forecast", x = "Year", y = "Evapotranspiration (inches)")

grid.arrange(pistachio_ET, Fallow_ET, ncol = 2)