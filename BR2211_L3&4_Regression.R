#BR2211 L3 & 4

# Remove the full list of R objects in session
rm(list = ls())
# Set data directory
setwd("/Users/joeyzhou/Library/CloudStorage/OneDrive-Personal/0.OneDrive_ZhouFiles/A.Work/A.NTU/A.Uni.Courses/BR2211 Financial and Risk Analytics I/2025 Material (Joey)/Lecture Slides/Lecture Data")

###########  R script ####################################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################


# R-example1 --------------------------------------------------------------


## Weekly interest rates - least-squares estimates

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
# Weekly interest rates from February 16, 1977, to December 31, 1993, were obtained from the Federal Reserve Bank of Chicago

# changes in 10-year treasury rate
cm10_dif = diff( cm10 )
# changes in AAA rated (Moody's) bond yields
aaa_dif = diff( aaa )

par(mfrow=c(1,1))
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate",ylab="change in AAA rate")
options(digits = 3)

#Linear regression function "lm"
summary(lm(aaa_dif ~ cm10_dif))



# R-example2 --------------------------------------------------------------

data(Capm,package="Ecdat")
attach(Capm)
#Excess return=return-rf

#excess monthly return of the food sector (rfood)
rfood2 = rfood/100
#excess monthly return of the market portfolio (rmrf)
rmrf2 = rmrf/100
plot(rmrf2,rfood2,ylab="Food industry excess return",xlab="Market excess return")
options(digits = 3)

summary(lm(rfood2~rmrf2))



# R-example3 --------------------------------------------------------------

#changes in the 30-year Treasury rate (cm30 dif)
cm30_dif = diff( cm30 )
#changes in the Federal funds rate (ff dif)
ff_dif = diff( ff )

options(digits = 3)
summary(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))

#Scatterplot matrix of the changes in four weekly interest rates
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))
#or can use another code as below:
pairs(~ . , panel=panel.smooth, data = as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)),cex.main = 0.9)


# R-example4 --------------------------------------------------------------

options(digits = 3)
anova(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
#changing the order of predictors
anova(lm(aaa_dif~ff_dif+cm30_dif+cm10_dif))

#ANOVA table is most useful for assessing the effects of adding predictors in some natural order. Since AAA bonds have maturities closer to 10 than to 30 years, and since the Federal Funds rate is an overnight rate, it made sense to order the predictors as cm10_dif, cm30_dif, and ff_dif as done initially.



# R-example5 --------------------------------------------------------------

fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
#testing 1-predictor vs 3-predictor
anova(fit1, fit3)
#testing 2-predictor vs 3-predictor
anova(fit2, fit3)


# R-example6 --------------------------------------------------------------

#Diagnostic plots

par(mfrow = c(2,2))
#plot(fit1)
plot(fit2)
#plot(fit3)


# R-example7 --------------------------------------------------------------
library(robust)
library("faraway")
set.seed(99)
x = 1:11
x[11] = 50
y=1+x+rnorm(11)
y2 = y
y2[11] = y[11]-45
x2 = x
x2[11] = 5.5
cexx = c(rep(21,10),19)

par(mfrow=c(1,3),lwd=1)
plot(x,y,ylim=c(0,60),cex=c(rep(1.25,10),1.5),pch=cexx,main="(a)")
abline(lm(y~x),lwd=2)
plot(x,y2,ylim=c(0,60),cex=c(rep(1.25,10),1.5),ylab="y",pch=cexx,main="(b)")
abline(lm(y2~x),lwd=2)
plot(x2,y,ylim=c(0,60),cex=c(rep(1.15,10),1.5),xlab="x",pch=cexx,main="(C)")
abline(lm(y~x2),lwd=2)


# (a) Linear regression with a high-leverage point that is not a residual outlier (solid circle). 
# (b) Linear regression with a high-leverage point that is a residual outlier (solid circle). 
# (c) Linear regression with a low-leverage point that is a residual outlier (solid circle). 
# Least-squares fits are shown as solid lines.

#checking leverage
par(mfrow=c(1,3),lwd=1,pch=19)
plot(hatvalues(lm(y~x)),ylab="leverage",main="(a)",ylim=c(0,1))
plot(hatvalues(lm(y2~x)),ylab="leverage",main="(b)",ylim=c(0,1))
plot(hatvalues(lm(y~x2)),ylab="leverage",main="(c)",ylim=c(0,1))

#checking externally studentized residuals
par(mfrow=c(1,3),lwd=1,pch=19)
plot(rstudent(lm(y~x)),ylab="studentized residual",main="Dataset (a)")
plot(rstudent(lm(y2~x)),ylab="studentized residual",main="Dataset (b)")
plot(rstudent(lm(y~x2)),ylab="studentized residual",main="Dataset (c)")
#checking externally raw residuals
#plot(residuals(lm(y~x)),ylab="residual",main="Dataset (a)")
#plot(residuals(lm(y2~x)),ylab="residual",main="Dataset (b)")
#plot(residuals(lm(y~x2)),ylab="residual",main="Dataset (c)")



# R-example8 --------------------------------------------------------------


bondprices = read.table("bondprices.txt",header=T)
attach(bondprices)

fit = nls(price~1000*exp(-r*maturity),start=list(r=.04))
summary(fit)

#Plot of bond prices against maturities with the predicted price from the nonlinear least-squares fit
par(mfrow=c(1,1))
plot(maturity,price,pch="*",cex = 2)
grid = seq(0, 20, length=201)
price_grid = 1000*exp(-0.0585*grid)
lines(grid,price_grid, lwd = 2, col = "red")
detach(bondprices)



# R-example9 --------------------------------------------------------------


DefaultData = read.table("DefaultData.txt",header=T)
attach(DefaultData)

#convert to frequencies and ln(freq)
freq2=freq/100
y = log(freq2[freq2>0])

#quick check the data pattern
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
##clearly non-linear

#fit linear model to log(freq): no default data is removed when modelling
fit_bow = lm(y ~ rating[freq>0])

#directly fit freq with non-linear exponential model, all data are used
fit_nls = nls(freq2 ~ exp(b1+b2*rating), start=list(b1=-5,b2=.5))

#fit with transform-both-sides method
fit_tbs = nls(sqrt(freq2) ~ exp(b1/2+b2*rating/2), start=list(b1=-6,b2=.5))

summary(fit_bow)
summary(fit_nls)
summary(fit_tbs)


sum_nls = summary(fit_nls)
coef_nls = as.numeric(sum_nls$coef[1:2])
sum_tbs = summary(fit_tbs)
coef_tbs = as.numeric(sum_tbs$coef[1:2])
rate_grid = seq(1,16,by=.01)
coef_bow = fit_bow$coefficients

#Plot the results

#Plot the data vs bow fitted
par(mfrow=c(1,3))
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_bow[1]+coef_bow[2]*rate_grid))
legend("topleft",c("BOW model","data"),lty=c(1,NA),pch=c("","*"),pt.cex=c(1, 1.5))


#Plot the data vs exponential fitted
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_nls[1]+coef_nls[2]*rate_grid))
legend("topleft",c("Non-linear","data"),lty=c(1,NA),pch=c("","*"),pt.cex=c(1, 1.5))

#Plot the data vs TBS
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_tbs[1]/2+coef_tbs[2]/2*rate_grid)^2)
legend("topleft",c("TBS","data"),lty=c(1,NA),pch=c("","*"),pt.cex=c(1, 1.5))


#Plot the log(freq) vs fitted models
par(mfrow=c(1,1))
plot(rate_grid, (coef_bow[1]+rate_grid*coef_bow[2]),
     type="l",ylim=c(-14.15,1),xlab="rating",ylab="log(default probability)")
lines(rate_grid,( coef_nls[1]+coef_nls[2]*rate_grid) ,lty=2,col="red")
lines(rate_grid,( coef_tbs[1]+coef_tbs[2]*rate_grid) ,lty=6,col="blue")
# When freq2=0, log(freq2) does not have values. For visual purposes, we add e^-6 so that they appear on the RHS graph.
points(rating,log(freq2+1e-6))
legend("topleft",c("BOW","Non-linear","TBS","data"),lty=c(1,2,6,NA),pch=c("","","","o"),col=c("black","red","blue"))


#(a) Residuals for estimation of default probabilities by nonlinear regression.
#(b) Normal probability plot of the residuals. 

par(mfrow=c(1,2))
fitted_nls = -sum_nls$resid+freq2
plot(fitted_nls,abs(sum_nls$resid),xlab="fitted values",ylab="absolute residual")
fit_loess  = loess(abs(sum_nls$resid)~ fitted_nls,span=1,deg=1)
ord_nls = order(fitted_nls)
lines(fitted_nls[ord_nls],fit_loess$fit[ord_nls])
qqnorm(sum_nls$resid,datax=T,main="",ylab="sample quantiles",xlab="theoretical quantiles")
qqline(sum_nls$resid,datax=T)

#(a)Substantial heteroskedasticity is indicated because the data on the left side are less scattered than elsewhere. 
#(b)Notice the outliers caused by the nonconstant variance.


# R-example10 --------------------------------------------------------------

##simulate data
n = 80
set.seed("781235")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)
y = (8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 

#Responses plotted against the two predictor variables
par(mfrow=c(1,2))
plot(x1,y,xlab=expression(x[1]))
plot(x2,y,xlab=expression(x[2]))


# fit simple linear model with x1 and x2
fit = lm(y~x1+x2)
# transform rstudent residuals
rstudent = rstudent(fit)

#Normal plot and histogram of the studentized residuals
par(mfrow=c(1,2))
qqnorm(rstudent,datax=T,main="Normal QQ Plot")
hist(rstudent,12)
#Normal plot and histogram of the studentized residuals.
#Right skewness is evident and perhaps a square root or log transformation of Y would be helpful.

#plot profile likelihood for \alpha using boxcox() function
library(MASS)
par(mfrow=c(1,1))
boxcox(y~poly(x1,2)+x2,ylab="log-likelihood")

# MLE for \alpha is somewhere near -1--> we choose \alpha=-1

yinv = -1/y
lm_bc = lm(yinv~poly(x1,2)+x2)
rstudent=rstudent(lm_bc)

#Residuals for the Box–Cox model applied to the simulated data
par(mfrow=c(2,2))
plot(lm_bc$fitted,rstudent,xlab="fitted values",main="(a)")
plot(x1,rstudent,main="(b)",xlab=expression(x[1]))
plot(x2,rstudent,main="(c)",xlab=expression(x[2]))
qqnorm(rstudent,datax=T,main="(d)",xlab="theoretical quantiles",ylab="sample quantiles")
qqline(rstudent,datax=T)

#(a) that there is no sign of heteroskedasticity, since the vertical scatter of the residuals does not change from left to right. 
#(b)&(c) we see uniform vertical scatter which shows that the model that is quadratic in X1 and linear in X2 fits −1/Yi well. 
#(d), we see that the residuals appear normally distributed



# R-example11 -------------------------------------------------------------

#GLM example

library("AER")
library("faraway")
library(MASS)
data("CreditCard") 

#1. card = Was the application for a credit card accepted?
#2. reports = Number of major derogatory reports
#3. income = Yearly income (in USD 10,000)
#4. age = Age in years plus 12ths of a year
#5. owner = Does the individual own his or her home?
#6. dependents = Number of dependents
#7. months = Months living at current address
#8. share = Ratio of monthly credit card expenditure to yearly income
#9. selfemp = Is the individual self-employed?
#10. majorcards = Number of major credit cards held
#11. active = Number of active credit accounts
#12. expenditure = Average monthly credit card expenditure

# checking basic features of data
attach(CreditCard)
par(mfrow=c(1,1)) 
hist(as.numeric(card),main="card",breaks=2,xlab=" no   yes")

par(mfrow=c(3,3)) 
hist(reports,main="reports")
hist(income, main="income")
hist(share, main="share")
hist(age, main="age")
hist(as.numeric(owner), main="owner",breaks=2,xlab=" no   yes",axes=F,ylab="")
h=hist(dependents,main="dependents",breaks=(0:7)-.5)
hist(months,main="months")
hist(log(share),main="log(share)")
hist(log(reports+1),main="log(reports+1)")

#age: outliers for age under 1 year --> Delete from future modelling
#reports: extremely right-skewed; most values of reports are 0 or 1 but the maximum value is 14. To reduce the skewness, log(reports+1) will be used instead of reports. he “1” is added to avoid taking the logarithm of 0.
#share: highly right-skewed, so log(share) will be used in the analysis


#Deleting age under 18
CreditCard_clean = CreditCard[CreditCard$age>18,]
attach(CreditCard_clean)

names(CreditCard)
fit1= glm(card~log(reports+1)+income+log(share)+age+owner+dependents+months,
          family="binomial",data=CreditCard_clean)
summary(fit1)
# Several predictors have large p-value

#stepAIC(fit1)
# Model selection test with AIC--> introduced later

#mean centering all selected variables log(reports + 1) + income + log(share) + dependents
log_reports_c = log(reports+1)
log_reports_c = log_reports_c - mean(log_reports_c)
income_c = income - mean(income)
log_share_c = log(share) - mean(log(share))
dependents_c = dependents - mean(dependents)

par(mfrow=c(4,2)) 
hist(income, main="income")
hist(income_c, main="income Centered")
hist(dependents,main="dependents")
hist(dependents_c,main="dep. Centered")
hist(log(share),main="log(share)")
hist(log_share_c,main="log(share) Centered")
hist(log(reports+1),main="log(reports+1)")
hist(log_reports_c,main="log(reports+1) Cencered")


#Re-fit the glm model
glm_fit02 = glm(card~log_reports_c+income_c+log_share_c+dependents_c,family="binomial",data=CreditCard_clean)
summary(glm_fit02)

#Plots of probabilities of a credit card application being accepted as functions of single predictors with other predictors fixed at their means

income_grid = seq(min(income),max(income),.01)
share_grid = exp(seq(min(log(share)),max(log(share)),.01))
share_grid2 = log(share_grid) - mean(log(share))
reports_grid = 0:14
reports_grid2 = log(reports_grid+1) - mean(log(reports+1))

par(mfrow=c(2,2))
plot(reports_grid, plogis(9.5238 -2.8953 * reports_grid2 ),type="b",
     lwd=2,xlab="reports",ylab="P(accept)",ylim=c(0,1))
plot(income_grid, plogis(9.5238 + 0.8717 *(income_grid-mean(income)) ),type="l",
     cex=2,lwd=2,xlab="income",ylab="P(accept)",ylim=c(0,1))
plot(log(share_grid), plogis(9.5238  + 3.3102  * share_grid2 ),type="l",
     cex=2,lwd=2,xlab="log(share)",ylab="P(accept)",ylim=c(0,1))
plot(0:6,plogis(9.5238 - .5506*((0:6)-mean(dependents)) ),type="b",
     lwd=2,xlab="dependents",ylab="P(accept)",ylim=c(0,1))

# the variable with the largest effect is share, the ratio of monthly credit card expenditure to yearly income.
# applicants who spend little of their income through credit cards are unlikely to have their applications accepted.

#Plots of log(share) versus other variables
par(mfrow=c(2,2))
plot(log(share),as.numeric(card)-1,ylab="card",main="(a)" )
plot(log(share),reports,main="(b)" )
plot(log(share),income,main="(c)" )
plot(log(share),majorcards,main="(d)" )

#(a) an application is always accepted if log(share) exceeds −6, which translates into share exceeding 0.0025. Thus, in this data set, among the group of applicants whose average monthly credit card expenses exceeded 0.25% of yearly income, all credit card applications were accepted.
#(c) a group of points following a smooth curve. This is a group of 316 applications who had the product of share times income exactly equal to 0.0012, the minimum value of this product.(share is never 0)

detach(CreditCard_clean)

