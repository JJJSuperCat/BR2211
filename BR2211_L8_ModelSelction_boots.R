#BR2211 L7

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
#Weekly interest rates from February 16, 1977, to December 31, 1993, were obtained from the Federal Reserve Bank of Chicago

#changes in 10-year treasury rate
cm10_dif = diff( cm10 )
#changes in AAA rated (Moody's) bond yields
aaa_dif = diff( aaa )
#changes in the 30-year Treasury rate (cm30 dif)
cm30_dif = diff( cm30 )
#changes in the Federal funds rate (ff dif)
ff_dif = diff( ff )

library(leaps)
subsets = regsubsets(aaa_dif~.,data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1)
b = summary(subsets)
b

par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables",
     ylab="BIC",cex=2.5)
plot(1:3,b$cp,type="b",xlab="number of variables",
     ylab="Cp",cex=2.5)
plot(1:3,b$adjr2,type="b",xlab="number of variables",
     ylab="adjusted R2")


# R-example2 --------------------------------------------------------------


#Scatterplot matrix of the changes in four weekly interest rates
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))
# high correlation between cm10_dif and cm30_dif
cor(cm10_dif,cm30_dif)

summary(lm(aaa_dif~cm10_dif))
summary(lm(aaa_dif~cm10_dif+cm30_dif))
summary(lm(aaa_dif~cm30_dif))

library(faraway)
options(digits=2)
vif(lm(aaa_dif~cm10_dif+cm30_dif+ff_dif))


# R-example3 --------------------------------------------------------------

# read Nelson-Plosser data set of U.S. yearly macroeconomic time series
nelsonplosser = read.csv("nelsonplosser.csv")

# check the data names
names(nelsonplosser)

# 1. sp     -Stock Prices, [Index; 1941-43 = 100], [1871-1970].
# 2. gnp.r  -Real GNP, [Billions of 1958 Dollars], [1909-1970],
# 3. gnp.pc -Real Per Capita GNP, [1958 Dollars], [1909-1970],
# 4. ip     -Industrial Production Index, [1967 = 100], [1860-1970],
# 5. cpi    -Consumer Price Index, [1967 = 100], [1860-1970],
# 6. emp    -Total Employment, [Thousands], [1890-1970],
# 7. bnd    -Basic Yields 30-year Corporate Bonds, [% pa], [1900-1970].

#Since two of the time series start in 1909, we use only the data from 1909 until the end of the series in 1970, a total of 62 years.

#omitting na values
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)
n = length(gnp.r)
year = 1909 + (1970-1909)*(0:(n-2))/n
year_1 = 1909 + (1970-1909)*(0:(n-1))/n

# initial check on trends
par(mfrow=c(3,4))
plot(year_1,sp,main="Stock Prices",xlab="year",type="l")
plot(year,diff(sp),main="Stock Prices",xlab="year",type="l")

plot(year_1,gnp.r,main="Real GNP",xlab="year",type="l")
plot(year,diff(gnp.r),main="Real GNP",xlab="year",type="l")

plot(year_1,gnp.pc,main="Real Per Capita GNP",xlab="year",type="l")
plot(year,diff(gnp.pc),main="Real Per Capita GNP",xlab="year",type="l")

plot(year_1,ip,main="Industrial Production Index",xlab="year",type="l")
plot(year,diff(ip),main="Industrial Production Index",xlab="year",type="l")

plot(year_1,cpi,main="Consumer Price Index",xlab="year",type="l")
plot(year,diff(cpi),main="Consumer Price Index",xlab="year",type="l")

plot(year_1,emp,main="Total Employment",xlab="year",type="l")
plot(year,diff(emp),main="Total Employment",xlab="year",type="l")

plot(year_1,bnd,main="Basic Yields 30-year Corporate Bonds",xlab="year",type="l")
plot(year,diff(bnd),main="Basic Yields 30-year Corporate Bonds",xlab="year",type="l")


#How does one decide whether to difference the original series, the log-transformed series, or some other function of the series?
# Aim: to stabilize the fluctuations in the differenced series.

par(mfrow=c(3,2),cex.lab=1.35)
plot(year,diff(gnp.r),type="b",ylab="differences",main="gnp.r")
plot(year,diff(log(gnp.r)),type="b",ylab="differences",main="log(gnp.r)")

plot(year,diff(ip),type="b",ylab="differences",main="ip")
plot(year,diff(log(ip)),type="b",ylab="differences",main="log(ip)")

plot(year,diff(cpi),type="b",ylab="differences",main="cpi")
plot(year,diff(log(cpi)),type="b",ylab="differences",main="log(cpi)")


#Model: 
# Response: differences of log(sp), i.e. log returns on stock
# Predictors: 2-7, with ip and cpi taking log transformation

sp_log_dif=diff(log(sp))
gnpc_dif=diff(gnp.r)
gnppc_dif=diff(gnp.pc)
ip_log_dif=diff(log(ip))
cpi_log_dif=diff(log(cpi))
emp_dif=diff(emp)
bnd_dif=diff(bnd)

#scatterplot to check correlations
plot(as.data.frame(cbind(sp_log_dif,gnpc_dif,gnppc_dif,ip_log_dif,cpi_log_dif,emp_dif,bnd_dif)))
#or can use another code as below:
pairs(~ . , panel=panel.smooth, data = as.data.frame(cbind(sp_log_dif,gnpc_dif,gnppc_dif,ip_log_dif,cpi_log_dif,emp_dif,bnd_dif)),cex.main = 0.9)

fit_lm=lm(sp_log_dif ~ gnpc_dif+gnppc_dif+ip_log_dif+cpi_log_dif+emp_dif+bnd_dif)
options(digits = 3)
summary(fit_lm)

#checking for VIF
print(vif(fit_lm),digits=2)
cor(gnpc_dif,gnppc_dif)

#Model reduction using AIC
library(MASS)
step_lm = stepAIC(fit_lm)
summary(lm(step_lm))

#Calculating C_p to further investigation
x1= as.matrix(cbind(gnpc_dif,gnppc_dif,ip_log_dif,cpi_log_dif,emp_dif,bnd_dif))
names_x1 = c("gnp.r", "gnp.pc","log(ip)","log(cpi)","emp","bnd")
leaps.fit = leaps(y = sp_log_dif,x=x1,names=names_x1,nbest=1)
options(digits=2)
cbind(leaps.fit$which,leaps.fit$Cp)

#Model selection:linear model with ip_log_dif and bnd_dif
summary(lm(sp_log_dif ~ ip_log_dif + bnd_dif))

fit_lm_final=lm(sp_log_dif ~ ip_log_dif + bnd_dif)
par(mfrow = c(2,2))
plot(fit_lm_final)



# R-example4 --------------------------------------------------------------

library("AER")
library(caTools)

data("CreditCard") 

#set the random number sequence for random split
set.seed(2014)
#stratify on Y and randomly split data into train vs test set based on split ratio
train <- sample.split(Y=CreditCard$card,SplitRatio = 0.7)
trainset <- subset(CreditCard,train==T)
testset <- subset(CreditCard,train==F)

prop.table(table(trainset$card))

prop.table(table(testset$card))




# R-example5 --------------------------------------------------------------
midcapD.ts = read.csv("midcapD.ts.csv")
x = 100*as.matrix(midcapD.ts[,-c(1,22)])
train = x[1:250,]
valid = x[-(1:250),]
meansTrain = apply(train,2,mean)
commonmeanTrain = mean(meansTrain)
meansValid = apply(valid,2,mean)

#sum of squared errors
SSseparate = sum((meansTrain-meansValid)^2)
SScommon = sum((commonmeanTrain - meansValid)^2)

SSseparate
SScommon
#the common-mean estimator is much more accurate than using separate means

SScommonTrain = sum((commonmeanTrain - meansTrain)^2)
SScommonTrain



# R-example6: bootstrap ---------------------------------------------------

library(bootstrap) 
library(MASS)    #  For fitdistr
set.seed("3857")
data(CRSPday,package="Ecdat")
ge = CRSPday[,4]
nboot = 1000
options(digits=3)

#fitting t-distribution to the ge data: mu, sigma nu
t_mle = function(x){as.vector(fitdistr(x,"t")$estimate)}
t1=proc.time()

results = bootstrap(ge,nboot,t_mle)
t2=proc.time()

t2-t1

#fit t-distribution using MLE method: estimate parameters and SE
fitdistr(ge,"t") 

# bootstrap mean
rowMeans(results$thetastar[,])
# bootstrap SE
apply(results$thetastar[,],1,sd)


#bootstrap with smaller sample size
ge_250 = ge[1:250]
results_250 = bootstrap(ge_250,nboot,t_mle)

fitdistr(ge_250,"t")
apply(results_250$thetastar,1,mean) 
apply(results_250$thetastar,1,sd)   


par(mfrow=c(1,2))
plot(density(results_250$thetastar[3,]),xlab="df",
     xlim=c(2,21),main="(a) n = 250")
plot(density(results$thetastar[3,]),xlab="df",
     xlim=c(2,21),main="(b) n = 2528")

