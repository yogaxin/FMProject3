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

CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs

# Create a dummy variable for up days
CSAD.df$Dup = ifelse(CSAD.df$Rm > 0, 1, 0)

# Create interaction terms
CSAD.df$DupRm = CSAD.df$Dup * abs(CSAD.df$Rm)
CSAD.df$DupRm2 = CSAD.df$Dup * CSAD.df$Rm2
CSAD.df$DownRm = (1 - CSAD.df$Dup) * CSAD.df$Rm
CSAD.df$DownRm2 = (1 - CSAD.df$Dup) * CSAD.df$Rm2

# Estimate the model
model = lm(CSAD ~ DupRm + DownRm + DupRm2 + DownRm2, data = CSAD.df)

# Print the model summary
summary(model)

y = CSAD.df$CSAD
x1 = CSAD.df$DupRm
x2 = CSAD.df$DownRm
x3 = CSAD.df$DupRm2
x4 = CSAD.df$DownRm2


#Linear model
linearMod <- lm(y ~ x1 + x2 + x3 + x4)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

## Newey-West Heteroscedasticity and Autocorrelation consistent (HAC) estimators
coeftest(linearMod,vcov=NeweyWest(linearMod,verbose=T))

## Time varying regression models 
require (tvReg)
tvlm.fit = tvLM(y ~ x1 + x2 + x3 + x4, bw = NULL) #bw=0.09563651  
head (tvlm.fit$coefficients)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], type="l")
plot(tvlm.fit$coefficients[,4], type="l")

# AIC and BIC
sat.lm2 <- lm(y ~ x1 + x2 + x3 + x4) # estimate the regression with 4 variables

#AIC and BIC for 4 variable regression
AIC(sat.lm2, k=2)
BIC (sat.lm2)

# Bayesian models
library (brms)
daily = cbind(y, x1, x2, x3, x4)
model = brm(formula = y ~ x1 + x2 + x3 + x4, 
            data    = daily,
            seed    = 123)
summary(model)

# Markow regime-switching model
library (MSwM)

nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 6)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) #show the 
plotProb(msEuro ,which=1)
