main <- read.csv("dataAB.csv", header=TRUE)

head(main)
dim(main)
str(main)
summary(main)
colSums(is.na(main))

library(forecast)
library(Metrics)
library(lmtest)
library(urca)
library(vars)
library(tsDyn)
library(tseries)
library(ggplot2)

Cases <- main$Case
Rainfall <- main$Rainfall
Tmax <- main$Tmax
Tmin <- main$Tmin
Tmean <- main$Tmean
Humidity <- main$Humidity



main2 <- cbind(Cases, Rainfall, Tmax, Tmin, Tmean, Humidity)
main3 <- cbind(Cases, Tmax, Rainfall, Humidity)

TimeSeriesCase <- ts(
  Cases,
  frequency = 52,
  start = c(2023,1)
)
TimeSeriesRf <- ts(
  Rainfall,
  frequency = 52,
  start = c(2023,1)
)

TimeSeriesTMax <- ts(
  Tmax,
  frequency = 52,
  start = c(2023,1)
)

TimeSeriesTMin <- ts(
  Tmin,
  frequency = 52,
  start = c(2023,1)
)

TimeSeriesTMean <- ts(
  Tmean,
  frequency = 52,
  start = c(2023,1)
)

TimeSeriesHumidity <- ts(
  Humidity,
  frequency = 52,
  start = c(2023,1)
)
plot(
  seq_along(TimeSeriesCase),
  as.numeric(TimeSeriesCase),
  type = "l",
  xaxt = "n",
  xlim = c(0,140),
  main = "Weekly Animal-Bite TimeSeriesCases (140 WEEKS)",
  xlab = "Week",
  ylab = "Animal Bite TimeSeriesCases (140 WEEKS)",
  col = "steelblue"
)
axis(1, at = seq(0,140, by =20))
