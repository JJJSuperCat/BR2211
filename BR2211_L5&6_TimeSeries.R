#BR2211 L5 & 6

# Remove the full list of R objects in session
rm(list = ls())
# Set data directory
setwd("/Users/joeyzhou/Library/CloudStorage/OneDrive-Personal/0.OneDrive_ZhouFiles/A.Work/A.NTU/A.Uni.Courses/BR2211 Financial and Risk Analytics I/2025 Material (Joey)/Lecture Slides/Lecture Data")


###########  R script ####################################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

#install.packages("TeachingDemos")
#install.packages("Ecfun")

# R-example1 --------------------------------------------------------------

library(Ecdat)

#Example 12.1 Inflations and changes in inflation rates - time series plots
##Time series plot:
##1-month inflation rate
##2-first differences in 1-month inflation rate

?Mishkin
data(Mishkin,package="Ecdat")
head(Mishkin) #pai1 = one-month inflation rate (in percent, annual rate) 
y = as.ts(Mishkin[,1], start=1950, frequency=12) 
#y = ts(as.vector(Mishkin[,1]), start=1950, frequency=12)  
y

par(mfrow=c(2,1))
plot(y,ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(a)")
#Explain: it wander without reverting to a fixed mean; or only slowly reverting to a mean of approximately 4% -> not stationary

plot(diff(y),ylab="Change in Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.2,main="(b)")
#Explain:oscillate around a fixed mean of 0%. Likely to be stationary.
#Explain: (b) seems more suitable for modeling as stationary.

# Example 12.2 Air passenger
##Time series plot of monthly totals of air passengers (in thousands)

?AirPassengers
data(AirPassengers) # monthly total international airline passengers
AirPassengers
z = as.ts(AirPassengers, start=1949, frequency=12) 
z

#
par(mfrow=c(1,1))
plot(z,type="b",ylab="Passengers",cex.axis=1.5,cex.lab=1.5,cex=1.5,lwd=2)
#Explain: upward trend; seasonal variation with local peaks in summar and troughs in winter months; the seasonal oscillations increase over time.



# R-example2 --------------------------------------------------------------

#Example 12.3 Inflation rates and changes in the inflation rate - sample ACF plots and the Ljung-Box test
##Sample ACF plots of the one-month inflation rate (a) and changes in the inflation rate (b)

y = as.vector(Mishkin[,1]) 
head(y)

#
par(mfrow=c(1,2))
plot(y,ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="Inflation Rate")
plot(diff(y),ylab="Change in Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.2,main="Change in Rate")
acf(y,cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="Inflation Rate")
acf(diff(y),cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="Change in Rate")
pacf(y,cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="Inflation Rate")
pacf(diff(y),cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="Change in Rate")
#

#Box.test(diff(y), lag=10, type="Ljung-Box")


# R-example3 --------------------------------------------------------------

# Simulations of 200 observations from AR(1) processes with various values of alpha and μ = 0.

set.seed(8716)
e = rnorm(200)
x1 = x2 = x3 = x4 = e
for (t in 2:200){
  x1[t] = 0.98*x1[t-1]+e[t]
  x2[t] = -0.6*x2[t-1]+e[t]
  x3[t] = 1.00*x3[t-1]+e[t]
  x4[t] = 1.01*x4[t-1]+e[t]
}

#
par(mfrow=c(2,2),cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(x1,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(alpha," = 0.98")))
plot(x2,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(alpha == - 0.6)))
plot(x3,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(alpha," = 1")))
plot(x4,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(alpha," = 1.01")))
#


# R-example4 --------------------------------------------------------------

#Autocorrelation functions of AR(1) processes with alpha equal to 0.95, 0.75,0.2, and −0.9

alpha = c(0.95, 0.75, 0.2, -0.9)
#
par(mfrow=c(2,2))
for(i in 1:4){
  y = alpha[i]^(0:15)
  plot(0:15, y, xlab="k", ylab=expression(rho(k)), ylim=c(-1,1), type = 'l')
  points(0:15, y, pch = 8)
  text(10, -0.85, eval(substitute(expression(paste(alpha," = ",j)), list(j = as.character(alpha[i])))), cex = 1.1)
  abline(h=0)     
}
#



# R-example5 --------------------------------------------------------------

#Example: Daily log returns for BMW stock - Fitting AR(1) model

library(xts)
data(bmw,package="evir")
BMW = xts(bmw, attr(bmw,"times"))
?bmw

#
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(BMW,main="(a)", minor.ticks = FALSE)
acf(bmw,lag.max=20,main="(b)")
pacf(bmw,lag.max=20,main="(c)")
#
#(a) Daily log returns for BMW stock from January 1973 until July 1996, 
#(b) sample ACF 
#(c) sample PACF
#Lag 1 ACF out of test bound

options(digits=9)

# Ljung-Box test with lag of 5
Box.test(bmw, lag = 5, type = "Ljung-Box")

#fit AR(1) model
fitAR1 = arima(bmw, order = c(1,0,0))
print(fitAR1)
# alpha=0.081116, s.e.=0.012722;value is small but statistically significant to be non-zero
# A non-zero value of alpha means that there is some information in today’s return that could be used for prediction of tomorrow’s return, but a small value of alpha means that the prediction will not be very accurate.

# plotting the residuals and check whether it satisfies the "WHITE NOISE" conditions

#
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(xts(residuals(fitAR1), attr(bmw,"times")),main="(a)", minor.ticks = FALSE)
acf(residuals(fitAR1),lag.max=20,main="(b)")
qqnorm(residuals(fitAR1),main="(c)") ; qqline(residuals(fitAR1))
#
#(a) time series plot
#(b) sample ACF 
#(c) normal quantile plot of residuals from an AR(1) fit to the daily log returns for BMW stock.


Box.test(residuals(fitAR1), lag = 5, type = "Ljung-Box", fitdf = 1)
# The large p-value indicates that we should accept the null hypothesis that the residuals are uncorrelated, at least at small lags. This is a sign that the AR(1) model provides an adequate fit.

Box.test(residuals(fitAR1), lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(residuals(fitAR1), lag = 15, type = "Ljung-Box", fitdf = 1)
Box.test(residuals(fitAR1), lag = 20, type = "Ljung-Box", fitdf = 1)
# p-values values are “statistically significant” using the conventional cutoff of 0.05. The sample size is 6146, so it is not surprising that even a small amount of autocorrelation can be statistically significant. The practical significance of this autocorrelation is very doubtful.



# R-example6 --------------------------------------------------------------

# Inflation rate—AR(1) fit and checking residuals

data(Mishkin,package="Ecdat")
y = as.ts(Mishkin[,1], start=1950, frequency=12) 

fit = arima(y, order = c(1,0,0))

#plot ACF
par(mfrow=c(1,2))
acf(y,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")

#
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(residuals(fit),main="(a)")
acf(residuals(fit),lag.max=20,main="(b)")
qqnorm(residuals(fit),main="(c)") ; 
qqline(residuals(fit))
#
#Many ACF of the residuals fall out of the test bound

Box.test(fit$resid, type = "Ljung", lag = 24, fitdf = 1)
# p-value extremely small--> residual is not white noise



# R-example7 --------------------------------------------------------------

#ACF of three AR(2) processes; the legend gives the values of alpha1 and alpha2.

x1 = as.vector(ARMAacf(ar=c(0.5,-0.3), lag.max=10))
x2 = as.vector(ARMAacf(ar=c(0.5,0.15), lag.max=10))
x3 = as.vector(ARMAacf(ar=c(0.15,0.8), lag.max=10))

#
par(mfrow=c(1,1))
plot(0:10,x1,xlab="lag",ylab="ACF", main= "ACF of three AR(2) processes",cex.axis=1.5,
     cex.lab=1.5,cex=2,cex.main=1.5,pch="*",type="b",ylim=c(-.5,1))
lines(0:10,x2,cex.axis=1.5, cex.lab=1.5,cex=2,pch="o",type="b")
lines(0:10,x3,cex.axis=1.5, cex.lab=1.5,cex=2,pch="x",type="b")

abline(h=0)
legend("bottomright",c("(0.5, -0.3)", "(0.5, 0.15)","(0.15, 0.8)"), pch=c("*","o","x"), cex=1.5, box.lty=0)
#


# R-example8 --------------------------------------------------------------

#Changes in the inflation rate—AR(p) models

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 

#taking differences of y
x = diff(y)
logn = log(length(x))

#plot sample ACF and PACF to quickly check
par(mfrow=c(1,2))
acf(x,main="ACF: Changes in Inflation rate")
pacf(x,main="PACF: Changes in Inflation rate")

#fit AR(i) models for i=1,2,...,20, and plot the corresponding AIC, BIC

resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20){
  fit = arima(x,order=c(i,0,0))
  resultsdiff[i,1] = i  
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
}

#
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion",cex.lab=1.35,cex.axis=1.35,
     main="AIC and BIC for AR fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560),type='b')
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2,type='b')
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
#


options(digits=7)
# functions to automate the search for the AR model that optimizes AIC or other criteria (BIC etc).
library(forecast)
auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "aic")
auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "bic")



# R-example9 --------------------------------------------------------------


# model the inflation rate (not the changes) instead

par(mfrow=c(1,2))
acf(y,main="ACF: Inflation rate")
pacf(y,main="PACF: Inflation rate")


#fit AR(i) models for i=1,2,...,20, and plot the corresponding AIC, BIC
results = matrix(0,nrow=20,ncol=3)
for (i in 1:20){
  fit = arima(y,order=c(i,0,0))
  results[i,1] = i  
  results[i,2] = fit$aic
  results[i,3] = results[i,2] + (logn-2)*i
}

#
plot(results[,1],results[,2],xlab="p",ylab="criterion",cex.lab=1.35,cex.axis=1.35,
     main="AIC and BIC for AR fits to inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560),type='b')
points(results[,1],results[,3],pch="o",cex=2,type='b')
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
#

options(digits=5)
auto.arima(y, max.p = 20, max.q = 0,  d = 0, ic = "aic")
auto.arima(y, max.p = 20, max.q = 0,  d = 0, ic = "bic")

#plot inflation rate series & ACF ; residual series from AR(7) fit & ACF of residuals

par(mfrow=c(2,2))
plot(ts(y, start=1950, frequency=12),ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(a)")
acf(y,main="(b)")
fitAR7 = arima(y,c(7,0,0))
plot(ts(fitAR7$resid, start=c(1950,2), frequency=12),ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(c)")
acf(fit$resid,main="(d)")
#



# R-example10 -------------------------------------------------------------

#Changes in the inflation rate—MA models

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 
x = diff(y)
logn = log(length(x))

#Fitting MA(q) models
resultsdiff = matrix(0,nrow=9,ncol=3)
for (i in 1:9){
  fit = arima(x,order=c(0,0,i))
  resultsdiff[i,1] = i  
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
}

#
par(mfrow=c(1,1))
plot(resultsdiff[,1],resultsdiff[,2],xlab="q",ylab="criterion",cex.lab=1.35,cex.axis=1.35,
     main="AIC and BIC for MA fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2445,2500),type='b')
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2,type='b')
legend(1.2,2498,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
#

fitMA3 = arima(diff(y), order = c(0,0,3))
fitMA3
Box.test(fitMA3$residual, lag = 5, type="Ljung", fitdf = 3)
Box.test(fitMA3$residual, lag = 10, type="Ljung", fitdf = 3)
Box.test(fitMA3$residual, lag = 15, type="Ljung", fitdf = 3)

fitMA2 = arima(diff(y), order = c(0,0,2))
fitMA2
Box.test(fitMA2$residual, lag = 5, type="Ljung", fitdf = 2)
Box.test(fitMA2$residual, lag = 10, type="Ljung", fitdf = 2)
Box.test(fitMA2$residual, lag = 15, type="Ljung", fitdf = 2)



# R-example11 -------------------------------------------------------------

#Changes in risk-free returns–ARMA models

library("forecast")
data(Capm,package="Ecdat")  
rf=Capm$rf
diffrf=diff(rf)
logn = log(length(diffrf))

par(mfrow=c(2,3))
plot(ts(rf),main="rf")
acf(rf,main="ACF:rf")
pacf(rf,main="PACF:rf")
plot(ts(diffrf),main="diffrf")
acf(diffrf,main="ACF:diffrf")
pacf(diffrf,main="PACF:diffrf")


#fit arima(2,1,0)-->ar(2) with one time differencing
arima(rf,c(2,1,0))

#AIC and BIC are shown for ARMA models with p, q = 0, 1, 2
#To improve the appearance of the table, 1290 was added to all AIC and BIC values
res = matrix(0,nrow=9,ncol=4)
i = 1
for (p in 0:2)
{
  for (q in 0:2)
  {
    res[i,1] = p
    res[i,2] = q
    fit = arima(diffrf,c(p,0,q))
    res[i,4] = AIC(fit,k=logn) +1290
    res[i,3] = AIC(fit) +1290
    i=i+1
  }
}
options(digits=3)
res 

#fitting arma(1,1)
par(mfrow=c(2,1))
acf(diffrf)
bestfit =  arima(diffrf,c(1,0,1))
acf(bestfit$residual)


#
layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
acf(bestfit$residual,lag.max=20,main="(a)")
qqnorm(bestfit$resid,datax=T,main="(b)") ; qqline(bestfit$resid,datax=T)
plot(ts(bestfit$resid, start = c(1960,2), frequency = 12),ylab="Residual",main="(c)")
#



# R-example12 -------------------------------------------------------------

#AR(1) process, with I(0), I(1), and I(2)

set.seed(4631)
y1 = arima.sim(n = 500, list(ar = c(0.4)))
y2 = cumsum(y1)
y3 = cumsum(y2)

#
par(mfrow=c(3,1))
plot(y1,type="l",ylab=expression(y[1]),lwd=1,main="(a)")
plot(y2,type="l",xlab="Time",ylab=expression(y[2]),lwd=1,main="(b)")
plot(y3,type="l",xlab="Time",ylab=expression(y[3]),lwd=1,main="(c)")
#



# R-example13 -------------------------------------------------------------

#Fitting an ARIMA model to CPI data

CPI.dat = read.csv("CPI.dat.csv")
CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
CPI_diff2 = as.matrix(diff(log(CPI),diff=2))

#Time series plot of the log(CPI) data, with 1&2 times differencing
#
par(mfrow=c(3,1),cex.axis=1.3,cex.lab=1.1,cex.main=1.35, mgp=c(3,1,0))
plot(ts(log(CPI),start = c(1977,1), frequency = 12),xlab="year",ylab="log(CPI)",type="b",main="(a)")
plot(ts(as.vector(CPI_diff1),start = c(1977,2), frequency = 12),xlab="year",
     ylab=expression(paste(Delta," log(CPI)")),type="b",main="(b)")
plot(ts(as.vector(CPI_diff2),start = c(1977,3), frequency = 12),xlab="year",
     ylab=expression(paste(Delta^2," log(CPI)")),type="b",main="(c)")
#

# Fitting ARIMA(0,2,2) MODEL
fit_ma = arima(CPI_diff2,order=c(0,0,2))
Box.test(fit_ma$resid,lag=20,type="Ljung",fitdf=2)

#
par(mfrow=c(2,2),cex.axis=1.35,cex.lab=1.35,cex.main=1.35)
acf(log(CPI),main="(a) log(CPI)")
acf(CPI_diff1,main=expression(paste("(b) ",Delta," log(CPI}")))
acf(CPI_diff2,main=expression(paste("(c) ",Delta^2," log(CPI}")))
acf(fit_ma$resid,main="(d) residuals, ARIMA(0,2,2)")
#



# R-example14 -------------------------------------------------------------
#install.packages("forecast")
#library(forecast)

data(Mishkin,package="Ecdat")
y = as.vector(Mishkin[,1])
auto.arima(y, max.p = 2, max.q = 2,  d = 0, ic = "bic", trace = TRUE)

#If we fit stationary ARMA models to the inflation rates, then auto.arima() selects an ARMA(2,1,2) model

polyroot(c(1,-1.207, +0.224))
# both roots greater than 1 in absolute value, 1st root very close to 1, possible indication of non-stationarity



# R-example15 -------------------------------------------------------------


#Forecasting the one-month inflation rate

data(Mishkin,package="Ecdat")
y = as.vector(Mishkin[,1]) 

year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)

#From R-example 10, MA(3) fits well with diff(inflation)
fit_diff=arima(diff(y),c(0,0,3))

pred.infl_diff =predict(fit_diff, n.ahead = 100, newxreg = NULL,
                        se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)

#Plotting data and predictions
#
par(mfrow=c(1,1))
plot(year[t1],diff(y)[t1],xlim=c(1975,1999),ylim=c(-9,15),type="b",
     xlab="year",ylab="Change in inflation rate",cex.axis=1.5,cex.lab=1.5)
points(year[t2], pred.infl_diff$pred,type="p",pch="*")
lines(year[t2], pred.infl_diff$pred - 2*pred.infl_diff$se)
lines(year[t2], pred.infl_diff$pred + 2*pred.infl_diff$se)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))
#


#Directly fit ARIMA(0,1,3)

fit=arima(y,c(0,1,3))

# automatic package for predicting
pred.infl = predict(fit, n.ahead = 100, se.fit = TRUE)

t1 = 300:491
t2 = 492:(492+49+50)
year = seq(1950 + 1/12,2001+61/12,1/12)

#Plotting data and predictions
#
par(mfrow=c(1,1))
plot(year[t1],y[t1],ylim=c(-10,18),type="b",xlim=c(1975,1999),
     xlab="year",ylab="Inflation rate",cex.axis=1.15,cex.lab=1.15)
points(year[t2], pred.infl$pred,type="p",pch="*")
lines(year[t2], pred.infl$pred - 2*pred.infl$se)
lines(year[t2], pred.infl$pred + 2*pred.infl$se)
legend("bottomleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))
#


