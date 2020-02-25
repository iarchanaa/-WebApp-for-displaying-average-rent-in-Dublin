install.packages("dplyr")
install.packages("tidyr")
installed.packages("ggplot2")
library("dplyr")
library("tidyr")
library("ggplot2")

cloud_house_data <- read.csv(file="C:/Users/parth/OneDrive/Documents/DCU/Cloud/Final_Data.csv", sep=",", header=TRUE)

d1 <- filter(cloud_house_data, Area == "Dublin24")
print(d1)

d1<- d1 %>% unite("yearqtr", Year:Quarter, sep = "-")

#1BHK trend

myplot <- ggplot(data = d1, aes(x=yearqtr, y=X1_Bed_Avg_monthly_rent, group=1)) +
  geom_line() + labs(title="1 BHK quarterly trends", y="1 BHK Prices",x="Quarters")

myplot <- myplot + theme(axis.text.x = element_text(angle = -45, hjust = 0.001))
png(file="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_1bhk_trend.png")
myplot
dev.off()

#1BHK forecast

x<- ts(data = rev(d1$X1_Bed_Avg_monthly_rent), frequency = 4, start = c(2015,1))
autoplot(x)
model <- auto.arima(x)
future <- forecast(model,10)
png(filename="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_1bhk_forecast.png")
plot(future,showgap = FALSE, ylab = "1 bhk house price", xlab = "Time", main = "Arima forecast for Dublin24 1BHK")
dev.off()

#2BHK trend

myplot <- ggplot(data = d1, aes(x=yearqtr, y=X2_Bed_Avg_monthly_rent, group=1)) +
  geom_line() + labs(title="2 BHK quarterly trends", y="2 BHK Prices",x="Quarters")

myplot <- myplot + theme(axis.text.x = element_text(angle = -45, hjust = 0.001))
png(file="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_2bhk_trend.png")
myplot
dev.off()

#2BHK forecast

x<- ts(data = rev(d1$X2_Bed_Avg_monthly_rent), frequency = 4, start = c(2015,1))
autoplot(x)
model <- auto.arima(x)
future <- forecast(model,10)
png(filename="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_2bhk_forecast.png")
plot(future,showgap = FALSE, ylab = "2 bhk house price", xlab = "Time", main = "Arima forecast for Dublin24 2BHK")
dev.off()

#3BHK trend

myplot <- ggplot(data = d1, aes(x=yearqtr, y=X3_Bed_Avg_monthly_rent, group=1)) +
  geom_line() + labs(title="3 BHK quarterly trends", y="3 BHK Prices",x="Quarters")

myplot <- myplot + theme(axis.text.x = element_text(angle = -45, hjust = 0.001))
png(file="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_3bhk_trend.png")
myplot
dev.off()

#3BHK forecast

x<- ts(data = rev(d1$X3_Bed_Avg_monthly_rent), frequency = 4, start = c(2015,1))
autoplot(x)
model <- auto.arima(x)
future <- forecast(model,10)
png(filename="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_3bhk_forecast.png")
plot(future,showgap = FALSE, ylab = "3 bhk house price", xlab = "Time", main = "Arima forecast for Dublin24 3BHK")
dev.off()

#4BHK trend

myplot <- ggplot(data = d1, aes(x=yearqtr, y=X4_Bed_Avg_monthly_rent, group=1)) +
  geom_line() + labs(title="4 BHK quarterly trends", y="4 BHK Prices",x="Quarters")

myplot <- myplot + theme(axis.text.x = element_text(angle = -45, hjust = 0.001))
png(file="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_4bhk_trend.png")
myplot
dev.off()

#4BHK forecast

x<- ts(data = rev(d1$X4_Bed_Avg_monthly_rent), frequency = 4, start = c(2015,1))
autoplot(x)
model <- auto.arima(x)
future <- forecast(model,10)
png(filename="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_4bhk_forecast.png")
plot(future,showgap = FALSE, ylab = "4 bhk house price", xlab = "Time", main = "Arima forecast for Dublin24 4BHK")
dev.off()

#5BHK trend

myplot <- ggplot(data = d1, aes(x=yearqtr, y=X5_Bed_Avg_monthly_rent, group=1)) +
  geom_line() + labs(title="5 BHK quarterly trends", y="5 BHK Prices",x="Quarters")

myplot <- myplot + theme(axis.text.x = element_text(angle = -45, hjust = 0.001))
png(file="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_5bhk_trend.png")
myplot
dev.off()

#5BHK forecast

x<- ts(data = rev(d1$X5_Bed_Avg_monthly_rent), frequency = 4, start = c(2015,1))
autoplot(x)
model <- auto.arima(x)
future <- forecast(model,10)
png(filename="C:/Users/parth/OneDrive/Documents/DCU/Cloud/trendplots/Dublin24_5bhk_forecast.png")
plot(future,showgap = FALSE, ylab = "5 bhk house price", xlab = "Time", main = "Arima forecast for Dublin24 5BHK")
dev.off()