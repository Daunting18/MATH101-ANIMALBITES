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

#CREATION OF TIME SERIES OBJECTS 

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

#1 PLOTTING THE LINE GRAPH OF WEEKLY ANIMAL BITE CASES

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

#2 PLOTTING THE LINE GRAPH OF STANDARDIZED WEEKLY WEATHER VARIABLES USING ZSCORES
MainCompleteCases <- main[complete.cases(main),]
RainfallComplete <- MainCompleteCases$Rainfall
TmaxComplete <- MainCompleteCases$Tmax
TminComplete <- MainCompleteCases$Tmin
TmeanComplete <- MainCompleteCases$Tmean
HumidityComplete <- MainCompleteCases$Humidity

RainfallStandardized <- as.numeric(scale(RainfallComplete))
TmaxStandardized <- as.numeric(scale(TmaxComplete))
TminStandardized <- as.numeric(scale(TminComplete))
TmeanStandardized <- as.numeric(scale(TmeanComplete))
HumidityStandardized <- as.numeric(scale(HumidityComplete))

plot(
  RainfallStandardized,
  type = "l",
  ylim = range(c(RainfallStandardized, TmaxStandardized, TminStandardized, TmeanStandardized, HumidityStandardized)),
  main = "Weekly Weather Variables (Standardized Z-scores)",
  xlab = "Week",
  ylab = "Standardized Values",
  col = "steelblue",
  lwd = 1.5
)

lines(
  TmaxStandardized,
  col = "darkorange",
  lwd = 1.5
  )
lines(
  TminStandardized,
  col = "forestgreen",
  lwd = 1.5
)
lines(
  TmeanStandardized,
  col = "red",
  lwd =  1.5
)
lines(
  HumidityStandardized,
  col = "purple",
  lwd = 1.5
)

abline(h=0, col = "darkblue", lwd = 1.5)


#3 PLOTTING LINE GRAPH OF CASES +STANDARDIZED WEATHER VARIABLES
CaseStandardized <- as.numeric(scale(Cases))
CombineAllStandardizedVariables <- c(CaseStandardized, TmaxStandardized, TminStandardized, TmeanStandardized, HumidityStandardized)
#OPTIONAL IF WANT Y-axis(Ylim) to show -3 to 3 Z-score

plot(
  CaseStandardized,
  type = "l",
  xlim = c(0,140),
  ylim = c(-3,7),
  main = "Standardardized Cases and Weather Variables",
  xlab = "Week",
  ylab = "Standardized Values (Z-score)",
  col = "steelblue",
  lwd = 1.5
)
abline(h=0, col ="darkblue")
axis(1, at = seq(0,140, by =20))

lines(
  RainfallStandardized,
  col = "forestgreen"
)
lines(
  TmaxStandardized,
  col = "darkorange",
  lwd = 1.5
)
lines(
  HumidityStandardized,
  col = "red",
  lwd = 1.5
)

#CONDUCTING STATIONARY TESTS

CasesFirstDiff <- diff(Cases)
RainfallFirstDiff <- diff(Rainfall)
TmaxFirstDiff <- diff(Tmax)
TminFirstDiff <- diff(Tmin)
TmeanFirstDiff <- diff(Tmean)
HumidityFirstDiff <- diff(Humidity)

#4. PLOTTING FIRST DIFFERENCE CASES

plot(
  CasesFirstDiff,
  type = "l",
  col = "steelblue",
  lwd = 1.5,
  main = "First Difference of Weekly Animal-Bite Cases",
  xlim = c(0,140),
  xlab ="Week",
  ylab= "Delta Cases"
)

abline(h=0, col ="darkblue", lwd = 1.5)

#ADF AND KPSS TESTS

ADF_Level <- ur.df(
  Cases,
  type ="drift",
  lags = 1
  )
summary(ADF_Level)

KPSS_Level <- ur.kpss(
  Cases,
  type = "mu",
  lags="short",
)

summary(KPSS_Level)


#LEVEL SERIES TESTS DISAGREE WITH EACH OTHER, 
#ADF TEST SAY THAT IT IS STATIONARY BUT KPSS TEST SAY IT IS NONSTATIONARY



#ADF AND KPSS TEST USING FIRST DIFFERENCE

ADF_FIRSTDIFF <- ur.df(
  CasesFirstDiff,
  type = "drift",
  lags = 1
)

summary(ADF_FIRSTDIFF)

KPSS_FIRSTDIFF <- ur.kpss(
  CasesFirstDiff,
  type = "mu",
  lags = "short"
)

summary(KPSS_FIRSTDIFF)


#BOTH TESTS SUGGESTS THAT THE FIRST DIFFERENCED CASES SERIES IS STATIONARY


#STANDARD JOHANSSEN PROCEDURE SUGGESTS THAT IT IS PLAUSIBLY I(1) AS IT BECOMES STATIONARY UPON USING THE FIRST DIFFERENCED CASES
