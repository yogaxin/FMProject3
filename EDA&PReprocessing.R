# Stock Benchmark: S&P600
# Plot the chat for S&P600 from 2016-04-24 to 2023-04-24\
require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations
require(zoo)

library(tidyquant)

start_date = "2010-05-10"
end_date = "2023-05-10"

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
####################################################################################
# Stock candidate 1: Christian Dior SE (CDI.PA), Luxury company, Sector: Healthcare
# Get ticket of CDI
getSymbols("CDI.PA", from = start_date,
           to = end_date, periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE) # getting CDI.PA prices
CDI.PA <- na.omit(CDI.PA)

chart_Series(CDI.PA) #plotting the series
cdi_data.xts = CDI.PA$CDI.PA.Close

cdi_return = diff (log(cdi_data.xts)) # Log return calculation
cdi_return = cdi_return [-1] # removing the first empty observation, received after return calculation
summary (cdi_return)

plot(cdi_return, main = "Christian Dior daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (cdi_return) # calculate mean of the return
mean (cdi_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(cdi_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (cdi_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(cdi_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(cdi_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(cdi_return)

# Variance
var(cdi_return)
# Standard deviation
sd(cdi_return)
# Median Absolute Deviation -> compare with Std Dev
mad (cdi_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (cdi_return$CDI.PA.Close)
kurtosis(cdi_return$CDI.PA.Close)

# Estimates Based on Percentiles
quantile (cdi_return)
quantile (cdi_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (cdi_return, tauseq)

IQR (cdi_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
cdi_return.density = density(cdi_return) # estimate kernel density
plot(cdi_return.density) # plot the estimated density

#QQ
qqnorm(cdi_return)
qqline(cdi_return)

## Shapiro-Wilk test 
cdi_return2 = fortify.zoo(cdi_return)
shapiro.test(cdi_return2$CDI.PA.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(cdi_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (cdi_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (cdi_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(cdi_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(cdi_return2$CDI.PA.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (cdi_return2$CDI.PA.Close, plnorm)
# 输出到CSV文件
write.csv(CDI.PA, file = "G:\\BA资料\\FM\\IA\\CDI.PA.csv", row.names = FALSE)
####################################################################################
# Stock candidate 2: L'Oreal (OR.PA), Luxury company, Sector: Healthcare
# Get ticket of OR
getSymbols("OR.PA", from = start_date,
           to = end_date, periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE) # getting OR.PA prices
OR.PA <- na.omit(OR.PA)
write.csv(OR.PA, file = "G:\\BA资料\\FM\\IA\\OR.PA.csv", row.names = FALSE)
chart_Series(OR.PA) #plotting the series
or_data.xts = OR.PA$OR.PA.Close

or_return = diff (log(or_data.xts)) # Log return calculation
or_return = or_return [-1] # removing the first empty observation, received after return calculation
summary (or_return)

plot(or_return, main = "L'Oreal daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (or_return) # calculate mean of the return
mean (or_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(or_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (or_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(or_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(or_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(or_return)

# Variance
var(or_return)
# Standard deviation
sd(or_return)
# Median Absolute Deviation -> compare with Std Dev
mad (or_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (or_return$OR.PA.Close)
kurtosis(or_return$OR.PA.Close)

# Estimates Based on Percentiles
quantile (or_return)
quantile (or_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (or_return, tauseq)

IQR (or_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
or_return.density = density(or_return) # estimate kernel density
plot(or_return.density) # plot the estimated density

#QQ
qqnorm(or_return)
qqline(or_return)

## Shapiro-Wilk test 
or_return2 = fortify.zoo(or_return)
shapiro.test(or_return2$OR.PA.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(or_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (or_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (or_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(or_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(or_return2$OR.PA.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (or_return2$OR.PA.Close, plnorm)

####################################################################################
# Stock candidate 3: The Estée Lauder Companies Inc. (EL), Luxury company, Sector: Healthcare
# Get ticket of EL
getSymbols("EL", from = start_date,
           to = end_date, periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE) # getting EL prices
EL <- na.omit(EL)
write.csv(OR.PA, file = "G:\\BA资料\\FM\\IA\\EL.csv", row.names = FALSE)
chart_Series(EL) #plotting the series
el_data.xts = EL$EL.Close

el_return = diff (log(el_data.xts)) # Log return calculation
el_return = el_return [-1] # removing the first empty observation, received after return calculation
summary (el_return)

plot(el_return, main = "Estée Lauder daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (el_return) # calculate mean of the return
mean (el_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(el_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (el_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(el_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(el_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(el_return)

# Variance
var(el_return)
# Standard deviation
sd(el_return)
# Median Absolute Deviation -> compare with Std Dev
mad (el_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (el_return$EL.Close)
kurtosis(el_return$EL.Close)

# Estimates Based on Percentiles
quantile (el_return)
quantile (el_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (el_return, tauseq)

IQR (el_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
el_return.density = density(el_return) # estimate kernel density
plot(el_return.density) # plot the estimated density

#QQ
qqnorm(el_return)
qqline(el_return)

## Shapiro-Wilk test 
el_return2 = fortify.zoo(el_return)
shapiro.test(el_return2$EL.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(el_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (el_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (el_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(el_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(el_return2$EL.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (el_return2$EL.Close, plnorm)

####################################################################################
# Stock candidate 4: Shiseido Company, Limited  (SSDOY), Luxury company, Sector: Healthcare
# Get ticket of P&G
getSymbols("SSDOY", from = start_date,
           to = end_date, periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE) # getting SSDOY prices
SSDOY <- na.omit(SSDOY)
write.csv(OR.PA, file = "G:\\BA资料\\FM\\IA\\SSDOY.CSV", row.names = FALSE)
chart_Series(SSDOY) #plotting the series
ssdoy_data.xts = SSDOY$SSDOY.Close

ssdoy_return = diff (log(ssdoy_data.xts)) # Log return calculation
ssdoy_return = ssdoy_return [-1] # removing the first empty observation, received after return calculation
summary (ssdoy_return)

plot(ssdoy_return, main = "Shiseido daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (ssdoy_return) # calculate mean of the return
mean (ssdoy_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(ssdoy_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (ssdoy_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(ssdoy_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(ssdoy_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(ssdoy_return)

# Variance
var(ssdoy_return)
# Standard deviation
sd(ssdoy_return)
# Median Absolute Deviation -> compare with Std Dev
mad (ssdoy_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (ssdoy_return$SSDOY.Close)
kurtosis(ssdoy_return$SSDOY.Close)

# Estimates Based on Percentiles
quantile (ssdoy_return)
quantile (ssdoy_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (ssdoy_return, tauseq)

IQR (ssdoy_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernssdoy density
ssdoy_return.density = density(ssdoy_return) # estimate kernssdoy density
plot(ssdoy_return.density) # plot the estimated density

#QQ
qqnorm(ssdoy_return)
qqline(ssdoy_return)

## Shapiro-Wilk test 
ssdoy_return2 = fortify.zoo(ssdoy_return)
shapiro.test(ssdoy_return2$SSDOY.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(ssdoy_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (ssdoy_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (ssdoy_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(ssdoy_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(ssdoy_return2$SSDOY.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (ssdoy_return2$SSDOY.Close, plnorm)


####################################################################################
# Stock candidate 5: The Procter & Gamble Company (PG), Luxury company, Sector: Healthcare
# Get ticket of P&G
getSymbols("PG", from = start_date,
           to = end_date, periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE) # getting PG prices
PG <- na.omit(PG)
write.csv(OR.PA, file = "G:\\BA资料\\FM\\IA\\PG", row.names = FALSE)
chart_Series(PG) #plotting the series
pg_data.xts = PG$PG.Close

pg_return = diff (log(pg_data.xts)) # Log return calculation
pg_return = pg_return [-1] # removing the first empty observation, received after return calculation
summary (pg_return)

plot(pg_return, main = "P&G daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (pg_return) # calculate mean of the return
mean (pg_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(pg_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (pg_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(pg_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(pg_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(pg_return)

# Variance
var(pg_return)
# Standard deviation
sd(pg_return)
# Median Absolute Deviation -> compare with Std Dev
mad (pg_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (pg_return$PG.Close)
kurtosis(pg_return$PG.Close)

# Estimates Based on Percentiles
quantile (pg_return)
quantile (pg_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (pg_return, tauseq)

IQR (pg_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernpg density
pg_return.density = density(pg_return) # estimate kernpg density
plot(pg_return.density) # plot the estimated density

#QQ
qqnorm(pg_return)
qqline(pg_return)

## Shapiro-Wilk test 
pg_return2 = fortify.zoo(pg_return)
shapiro.test(pg_return2$PG.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(pg_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (pg_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (pg_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(pg_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(pg_return2$PG.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (pg_return2$PG.Close, plnorm)

####################################################################################
# Stock candidate 6: The Sally Beauty Holdings Inc Company (SBH), Luxury company, Sector: Healthcare
# Get ticket of SBH
getSymbols("SBH", from = start_date,
           to = end_date, periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE) # getting SBH prices
SBH <- na.omit(SBH)
write.csv(OR.PA, file = "G:\\BA资料\\FM\\IA\\SBH.csv", row.names = FALSE)
chart_Series(SBH) #plotting the series
sbh_data.xts = SBH$SBH.Close

sbh_return = diff (log(sbh_data.xts)) # Log return calculation
sbh_return = sbh_return [-1] # removing the first empty observation, received after return calculation
summary (sbh_return)

plot(sbh_return, main = "Coty daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (sbh_return) # calculate mean of the return
mean (sbh_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(sbh_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (sbh_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(sbh_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(sbh_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(sbh_return)

# Variance
var(sbh_return)
# Standard deviation
sd(sbh_return)
# Median Absolute Deviation -> compare with Std Dev
mad (sbh_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (sbh_return$SBH.Close)
kurtosis(sbh_return$SBH.Close)

# Estimates Based on Percentiles
quantile (sbh_return)
quantile (sbh_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (sbh_return, tauseq)

IQR (sbh_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernsbh density
sbh_return.density = density(sbh_return) # estimate kernsbh density
plot(sbh_return.density) # plot the estimated density

#QQ
qqnorm(sbh_return)
qqline(sbh_return)

## Shapiro-Wilk test 
sbh_return2 = fortify.zoo(sbh_return)
shapiro.test(sbh_return2$SBH.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(sbh_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (sbh_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (sbh_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(sbh_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(sbh_return2$SBH.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (sbh_return2$SBH.Close, plnorm)

#================================================================================
#merge the table depending on date
# 对齐时间序列对象到同一日期范围
aligned_data <- data.frame()  #将df设置为一个空数据框

aligned_data <- merge(CDI.PA, OR.PA)
aligned_data <- merge(aligned_data, EL)
aligned_data <- merge(aligned_data, SSDOY)
aligned_data <- merge(aligned_data, PG)
aligned_data <- merge(aligned_data, SBH)
colnames(aligned_data)
aligned_data <- na.omit(aligned_data)
# 获取各个股票的收盘价
CDI.Close <- Cl(aligned_data[, "CDI.PA.Close"])
OR.Close <- Cl(aligned_data[, "OR.PA.Close"])
EL.Close <- Cl(aligned_data[, "EL.Close"])
SSDOY.Close <- Cl(aligned_data[, "SSDOY.Close"])
PG.Close <- Cl(aligned_data[, "PG.Close"])
SBH.Close <- Cl(aligned_data[, "SBH.Close"])

# 将这些收盘价合并到一个数据框中
close.prices <- data.frame(Date=index(CDI.Close), CDI.Close, OR.Close, EL.Close, 
                           SSDOY.Close, PG.Close, SBH.Close)

# Rename Columns
names(close.prices) <- c("Date", "CDI.PA", "OR.PA", "EL", 
                         "SSDOY", "PG", "SBH")

# Remove rows with NA values
close.prices <- na.omit(close.prices)


# 输出到CSV文件
write.csv(close.prices, file = "G:\\BA资料\\FM\\IA\\ClosePrices.csv", row.names = FALSE)
#===========================================================================================
setwd("G:\\BA资料\\FM\\IA")
library (xts)
library (zoo)
library (PerformanceAnalytics)
require (sandwich)
require(lmtest)

## load the data

close.prices = read.csv("ClosePrices.csv", header = TRUE) #importing the data
# converting the first column into date format
# Example of converting a character column to a Date column
close.prices[,1] <- as.Date(close.prices[,1], format="%Y-%m-%d")
close.prices.xts <- xts(close.prices[,-1], order.by=close.prices[,1])
# convert the object into xts object (time series object)
close.prices.zoo  <- as.zoo(close.prices.xts) # convert the time series object into zoo object
############################################################################################################
## the model
############################################################################################################

## calculate the return
str(close.prices.xts)
for(i in 1:ncol(close.prices.xts)) {
  close.prices.xts[,i] <- as.numeric(close.prices.xts[,i])
}

return = Return.calculate( close.prices.xts , method = "log") # automatically calculate return



library(pastecs)
descriptive.stat.return = stat.desc(return) # descriptive statistics



# a function to create CSAD and Rm
exchange.herd = function(return) 
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}


f = exchange.herd(return) # calling the function "exchange.herd" that calculates CSAD and Rm
head (f) # show the first 6 rows

CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs
head (CSAD.df) # show the first 6 rows
tail (CSAD.df) # show the last 6 rows


# reassign my columns as Y and Xs to look better in the regression model
y = CSAD.df$CSAD  # reassign my columns as Y and Xs to look better in the regression model
x1 = abs (CSAD.df$Rm)
x2 = CSAD.df$Rm2


#Linear model
linearMod <- lm(y~x1+x2)  # build linear regression model on full data
print(linearMod)
summary(linearMod)






## For curious students advanced estimatimation 
#Newey-West Heteroscedasticity and Autocorrelation consistent (HAC) estimators
coeftest(linearMod,vcov=NeweyWest(linearMod,verbose=T))


## For curious students - if you wish you can take a look at the Time varying regression models 
## the package tvReg

# estimate TV Linear Regression
require (tvReg)
tvlm.fit = tvLM(y~x1+x2, bw = NULL  ) #bw=0.05357343 
head (tvlm.fit$coefficients)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], type="l")

# AIC and BIC
# regression with one variable
sat.lm2 <- lm(y~x1+x2) # estimate the regression with 2 varibles

#AIC and BIC for 2 variable regression
AIC(sat.lm2, k=2)
BIC (sat.lm2)


# Bayesian models
library (brms)
daily = cbind(y, x1, x2)
model = brm(formula = y ~ x1+x2, 
            data    = daily,
            seed    = 123)
summary(model)


# Markow regime-switching model
library (MSwM)

nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) #show the 
plotProb(msEuro ,which=1)


#Quantile regression
library (quantreg)
taus<-seq(from = .1, to = .9, by = .1) 
coef0 <- rq( y ~ x1+x2, tau=taus)
summary (coef0)



#============================================================================================================================
# Define stock tickers and date range
tickers <- c("CDI.PA", "OR.PA", "EL", "SSDOY", "PG", "SBH")

# Download adjusted closing prices and volume for multiple stocks
stock_prices <- lapply(tickers, function(ticker) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)[,6]
})

# Remove rows with NA values
stock_prices <- na.omit(stock_prices)

# Find common date range
common_dates <- Reduce(intersect, lapply(stock_prices, index))

# Filter each dataframe to the common dates
stock_prices <- lapply(stock_prices, function(df) {
  df[index(df) %in% common_dates, ]
})

# Remove rows with NA values
stock_prices <- lapply(stock_prices, na.omit)

# Check if all dataframes have the same number of rows
print(unique(unlist(lapply(stock_prices, nrow))))

# If the above command prints only one number, then all dataframes have the same number of rows.
# Now you can write the data to a CSV file.
write.csv(stock_prices, file = "G:\\BA资料\\FM\\IA\\StockPrices.csv", row.names = FALSE)

#=================================================================================================================================
# Calculate daily returns
daily_returns <- lapply(stock_prices, function(price) diff(log(price))[-1])


# Set parameters
num_days <- length(daily_returns[[1]])
lookback_period <- 10 # Moving average lookback period
threshold <- 0.01 # Threshold for making investment decisions
transaction_cost <- 0.002 # Transaction cost as a percentage of the investment value

# Estimate signal accuracy using a machine learning model
signal_accuracies <- lapply(seq_along(tickers), function(j) {
  # Prepare data for machine learning
  data <- stock_prices[[j]] %>% as.data.frame() %>% dplyr::mutate(id = 1:n())
  colnames(data) <- c("price", "id")
  data <- data %>% mutate(signal = ifelse(lag(price, 1) < price, 1, 0))
  data <- data %>% dplyr::mutate(across(starts_with("price"), list(lag_1 = ~lag(.x, 1), lag_2 = ~lag(.x, 2))))
  
  # Fit a logistic regression model
  model <- glm(signal ~ . - price - id, data = data, family = "binomial", na.action = na.exclude)
  
  # Predict signal accuracy on training data
  pred <- predict(model, data, type = "response")
  accuracy <- mean((pred > 0.5) == data$signal, na.rm = TRUE)
  accuracy
})

# Generate private signals: 1 for positive return (buy), 0 for negative return (sell)
set.seed(42)
true_values <- lapply(daily_returns, function(ret) ifelse(ret > 0, 1, 0))
private_signals <- mapply(function(tv, acc) {
  rbinom(length(tv), 1, ifelse(tv == 1, acc, 1 - acc))
}, true_values, signal_accuracies, SIMPLIFY = FALSE)

# Initialize decision matrix
decisions <- matrix(0, nrow = num_days, ncol = length(tickers))

# Calculate moving averages of past returns and technical indicators (e.g., RSI) for each stock
moving_average_returns <- lapply(daily_returns, function(ret) {
  rollapply(ret, lookback_period, mean, align = "right", fill = NA)
})

rsi_periods <- 14
rsis <- lapply(stock_prices, function(price) {
  RSI(price, n = rsi_periods, maType = "WMA")
})

optimize_portfolio <- function(returns, target_return, lambda = 0.5) {
  cov_matrix <- cov(returns)
  expected_returns <- colMeans(returns)
  n_assets <- ncol(returns)
  
  Dmat <- cov_matrix + lambda * diag(1, n_assets, n_assets)
  dvec <- -expected_returns
  Amat <- cbind(matrix(1, nrow = n_assets, ncol = 1), diag(n_assets))
  bvec <- c(1, rep(0, n_assets))
  meq <- 1
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
  weights <- result$solution
  return(weights)
}


# Function to calculate the likelihood ratio of the observed decisions
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest
}


# Simulate the decision-making process
for (i in seq_len(num_days)) {
  if (i <= lookback_period) {
    # Investors make decisions based solely on their private signal during the initial lookback period
    decisions[i, ] <- sapply(private_signals, function(signal) ifelse(signal[i] == 1, 1, 0))
  } else {
    # Calculate expected return for each stock
    expected_returns <- sapply(moving_average_returns, function(mar) mar[i])
    
    # Optimize the portfolio allocation
    returns_matrix <- do.call(cbind, daily_returns)
    colnames(returns_matrix) <- tickers
    optimized_weights <- optimize_portfolio(returns_matrix[(i - lookback_period):(i - 1), ], mean(expected_returns))
    
    # Subsequent investors consider the moving averages of past returns, technical indicators, and the decisions of others
    for (j in seq_along(tickers)) {
      decisions_so_far <- decisions[(i - lookback_period):(i - 1), j]
      likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracies[[j]])
      
      # Adjust the optimized weights based on the likelihood ratio, private signals, and transaction costs
      if (private_signals[[j]][i] == 1 && likelihood_ratio > 1) {
        decisions[i, j] <- optimized_weights[j] * (1 + min(1, (likelihood_ratio - 1 - transaction_cost) / 10)) # Normalized to [0, 1]
      } else if (private_signals[[j]][i] == 0 && likelihood_ratio < 1) {
        decisions[i, j] <- optimized_weights[j] * (1 - min(1, (1 - likelihood_ratio - transaction_cost) / 10)) # Normalized to [0, 1]
      } else {
        decisions[i, j] <- optimized_weights[j]
      }
    }
  }
}

# Results
print(paste("Tickers:", paste(tickers, collapse = ", ")))
print(paste("Date range:", start_date, "to", end_date))
print(paste("Signal accuracies (based on historical data):", paste(round(unlist(signal_accuracies), 2), collapse = ", ")))

for (i in seq_along(tickers)) {
  print(paste("Ticker:", tickers[i]))
  
  
  print(paste("True values (1: buy, 0: sell):", paste(true_values[[i]], collapse = ", ")))
  # This line displays the true values (1: buy, 0: sell) for each time period for the current stock ticker. 
  # These true values represent whether the stock was a good (1) or bad (0) investment during that specific period. 
  # This information is not available to the individuals when making their decisions, but it's useful for assessing 
  # the performance of the simulated decision-making process.
  
  print(paste("Private signals (1: buy, 0: sell):", paste(private_signals[[i]], collapse = ", ")))
  # This line shows the private signals (1: buy, 0: sell) of each individual for the current stock ticker. 
  # Each individual receives a private signal, which is an imperfect indication of the true value of the stock. 
  # The signal accuracy determines how likely the private signal is to match the true value.
  
  print(paste("Decisions (Portfolio allocation):", paste(round(decisions[, i], 2), collapse = ", ")))
  #This line presents the decisions made by the individuals for the current stock ticker. 
  # The decisions are based on each individual's private signal and the decisions of those before them. 
  # The decisions represent the portfolio allocation for the stock, where 1 indicates buying the stock and 0 
  # indicates not buying (or selling) the stock.
}




