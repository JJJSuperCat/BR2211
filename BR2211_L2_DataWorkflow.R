#BR2211 L2

# Remove the full list of R objects in session
rm(list = ls())
###JZ: clears out all user-defined objects, effectively resetting your environment to a blank state.

# Set data directory
data_folder <- "/Users/joeyzhou/Library/CloudStorage/OneDrive-Personal/0.OneDrive_ZhouFiles/A.Work/A.NTU/A.Uni.Courses/BR2211 Financial and Risk Analytics I/2025 Material (Joey)/Lecture Slides/Lecture Data"


#install.packages("data.table","Ecdat")
#Easier to simply click and install all required packages using the RStudio interface
###JZ: Ecdat is an R package on CRAN that provides a collection of data sets for econometrics. It contains a variety of real-world and simulated data commonly used for teaching and research in econometrics, including data featured in well-known textbooks. Researchers, students, and practitioners often use the Ecdat package to quickly access classic econometric data sets for exercises, demonstrations, and examples. By installing and loading Ecdat, one can conveniently explore and analyze these data sets alongside econometric models and methods in R.

###########  R script ####################################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################


# R-example1 --------------------------------------------------------------

##Histogram##

##Data import method 1: from the package
#data(SP500, package = "Ecdat")
#?SP500

#Data import method 2: Import data from a csv file
library(readr)
###JZ:  loads the readr package into your R session. The readr package provides a set of fast and user-friendly functions for reading various text-based data files (such as CSV, TSV, or delimited files). Once loaded, you can use functions like read_csv(), read_tsv(), etc. for data import.
filename <- "SP500.csv"
SP500 <- read_csv(paste(data_folder, filename, sep = "/"))
###JZ: Daily log return on S&P 500 index from Jan 1981 to April 1991. The data set is the variable r500 in the SP500.

SPreturn <- SP500$r500
###JZ: SP500 is a data frame. $r500 selects the column named r500 within that data frame. SPreturn <- assigns that column to the new object SPreturn. So effectively, SPreturn becomes a vector (or column) containing the values from the r500 column of SP500.


#library(evir)  # for emplot
###JZ: No need this code for now, as we don't need it. The evir package  provides specialized functions for extreme value theory and volatility analysis.

#data(Garch,package="Ecdat")
#attach(Garch)
filename <- "Garch.csv"
Garch <- read_csv(paste(data_folder, filename, sep = "/"))
###JZ: Daily changes in the DM/dollar exchange rate, Jan 2, 1980 to May 21, 1987. The DM/dollar exchange rate is the variable dm
diffdm = diff(Garch$dm)  #  Deutsch mark
###JZ: diff() computes the day-to-day (or row-to-row) changes


# data(Capm, package = "Ecdat")
filename <- "Capm.csv"
Capm <- read_csv(paste(data_folder, filename, sep = "/"))
###JZ: The risk-free rate, Jan 1960 to Dec 2002. The rates are the variable rf in the Capm series.
diffrf = diff(Capm$rf)
###JZ: diff() computes the day-to-day (or row-to-row) changes

n = length(SPreturn)
n2 = length(diffdm)
n3 = length(diffrf)
Reldiffrf = diffrf/(Capm$rf[2:(n3+1)])

year_SP = 1981 + (1:n)* (1991.25-1981)/n
year_dm = 1980 + (1:n2)* (1987.5-1980)/n2
year_rf = 1960 + (1:n3) * (2003 - 1960)/n3

par(mfrow=c(3, 1))
###JZ: Sets up the graphics window to display plots in 3 rows and 1 column. The next two plot() calls will appear one above the other.

plot(year_SP,SPreturn,main="S&P 500 daily returns",xlab="year",type="l",
     ylab="log return")
###JZ: Notice the extreme volatility in Octber 1997

plot(year_dm,diffdm,xlab="year",ylab="change in rate",main=
       "changes inDM/dollar exchange rate",type="l")

plot(year_rf,diffrf,main="changes in risk-free interest return",xlab="year",
     ylab="change in rate",type="l")


par(mfrow=c(2,2))
###JZ: Sets up the graphics window to display plots in 2 rows and 2 column. 

hist(SPreturn,breaks=30,xlab="return",
     main="(a) 30 cells, full range")
hist(SPreturn,breaks=30,main="(b) 30 cells, central range",
     xlim=c(-.04,.04),xlab="return") #,cex.main=1.5)
hist(SPreturn,breaks=20,main="(c) 20 cells, central range",
     xlim=c(-.04,.04),xlab="return",)
hist(SPreturn,breaks=50,main="(d) 50 cells, central range",
     xlim=c(-.04,.04), xlab="return")


# R-example2 --------------------------------------------------------------

##Sample CDF##

set.seed("991155")
###JZ: Sets the random-number seed to ensure reproducibility of results.
###JZ: Whenever you use random-number-generating functions (e.g., rnorm()), R uses a pseudo-random number generator. Calling set.seed() with a specific number ensures that every time you run this code, you get the same sequence of random numbers.

edf_norm= ecdf(rnorm(150))
###JZ:Creates an empirical distribution function (EDF) of 150 random numbers drawn from a standard normal distribution.

par(mfrow=c(1,1))
###JZ: Sets up the graphics parameter so that subsequent plots are placed in a 1×1 grid.

plot(edf_norm, verticals= TRUE, do.p = F,main="EDF and CDF")

tt=seq(from=-3,to=3,by=.01)
lines(tt,pnorm(tt),lty=2,lwd=2,col="red")
legend(1.5,.2,c("EDF","CDF"),lty=c(1,2),lwd=c(1.5,2),col=c("black","red"))



# R-example3 --------------------------------------------------------------

##QQPLOT##

x = (1:299)/300
f1  = exp(3*x)
f2 = log(x+.01)
f3 = qnorm(x)
f4 = pnorm(6*x -3)
par(mfrow=c(2,2))
plot(x,f1,type="l",main="Convex", xlab="sample quantile", ylab="normal quantile")
plot(x,f2,type="l",main="Concave", xlab="sample quantile", ylab="normal quantile")
plot(x,f4,type="l",main="Convex-concave", xlab="sample quantile", ylab="normal quantile")
plot(x,f3,type="l",main="Concave-convex", xlab="sample quantile", ylab="normal quantile")

##QQplot for normal samples##

par(mfrow=(c(3,2)))
set.seed("543")
x1=rnorm(20)
qqnorm(x1,datax=T,main="n = 20")
qqline(x1,datax=T)
x1=rnorm(20)
qqnorm(x1,datax=T,main="n = 20")
qqline(x1,datax=T)
x1=rnorm(150)
qqnorm(x1,datax=T,main="n = 150")
qqline(x1,datax=T)
x1=rnorm(150)
qqnorm(x1,datax=T,main="n = 150")
qqline(x1,datax=T)
x1=rnorm(1000)
qqnorm(x1,datax=T,main="n = 1000")
qqline(x1,datax=T)
x1=rnorm(1000)
qqnorm(x1,datax=T,main="n = 1000")
qqline(x1,datax=T)


##QQplot for lognormal samples##

par(mfrow=(c(3,2)))
set.seed("864")
x1=rlnorm(150,sd=1)
qqnorm(x1,datax=T,main=expression(paste("n=150, ", sigma, " = 1"))) 
qqline(x1,datax=T)
x1=rlnorm(1000,sd=1)
qqnorm(x1,datax=T,main=expression(paste("n=1000, ", sigma, " = 1"))) 
qqline(x1,datax=T)
x1=rlnorm(150,sd=1/2)
qqnorm(x1,datax=T,main=expression(paste("n=150, ", sigma, " = 1/2"))) 
qqline(x1,datax=T)
x1=rlnorm(1000,sd=1/2)
qqnorm(x1,datax=T,main=expression(paste("n=1000, ", sigma, " = 1/2"))) 
qqline(x1,datax=T)
x1=rlnorm(150,sd=1/5)
qqnorm(x1,datax=T,main=expression(paste("n=150, ", sigma, " = 1/5"))) 
qqline(x1,datax=T)
x1=rlnorm(1000,sd=1/5)
qqnorm(x1,datax=T,main=expression(paste("n=1000, ", sigma, " = 1/5"))) 
qqline(x1,datax=T)


##QQplots for t samples##

par(mfrow=(c(3,2)))
set.seed("7290")
x1=rt(150,df=4)
qqnorm(x1,datax=T,main="n=150, df=4") 
qqline(x1,datax=T)
x1=rt(1000,df=4)
qqnorm(x1,datax=T,main="n=150, df=4") 
qqline(x1,datax=T)
x1=rt(150,df=10)
qqnorm(x1,datax=T,main="n=150, df=10") 
qqline(x1,datax=T)
x1=rt(1000,df=10)
qqnorm(x1,datax=T,main="n=150, df=10") 
qqline(x1,datax=T)
x1=rt(150,df=30)
qqnorm(x1,datax=T,main="n=150, df=30") 
qqline(x1,datax=T)
x1=rt(1000,df=30)
qqnorm(x1,datax=T,main="n=150, df=30") 
qqline(x1,datax=T)


##QQplot for artifitial Trimodal density##

set.seed(997733)
x = c(rnorm(250,mean=6),rnorm(150,mean=1),rnorm(250,mean = 12))
par(mfrow=c(1,2))
plot(density(x))
qqnorm(x,datax=T)
qqline(x,datax=T)


# R-example4 --------------------------------------------------------------


library(faraway) # for halfnorm plot

##Half-normal plot##

par(mfrow=c(1,1))
halfnorm(abs(diffrf - mean(diffrf)), main="changes in risk free rate",
         ylab="Sorted data")
?halfnorm


# R-example5 --------------------------------------------------------------

##QQPlot t distribution##

par(mfrow=c(3,2))

qqnorm(SPreturn,datax=TRUE,xlab="normal quantiles",ylab="Data",
       main="(a) Normal probability plot")
qqline(SPreturn,datax=TRUE)

s_SPreturn = sort(SPreturn)
grid = (1:n)/(n+1)
qqplot(s_SPreturn, qt(grid,df=1),main="(b) t-probability plot, 
       df = 1",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 1) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

qqplot(s_SPreturn, qt(grid,df=2),main="(c) t-probability plot, 
       df = 2",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 2) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)


qqplot(s_SPreturn, qt(grid,df=4),main="(d) t-probability plot, 
       df = 4",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 4) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

qqplot(s_SPreturn, qt(grid,df=8),main="(e) t-probability plot, 
       df = 8",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 8) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)


qqplot(s_SPreturn, qt(grid,df=15),main="(f) t-probability plot, 
       df = 15",xlab="Data",ylab="t-quantiles")
lines(quantile(s_SPreturn,c(.25,.75)),qt(c(.25,.75),df=15))
lmfit = lm( qt(c(.25,.75),df = 15) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)



# R-example6 --------------------------------------------------------------

##Boxplots##

par(mfrow=c(1,2))
boxplot(list(SPreturn,diffrf),boxwex=.5,
        names=list("S&P 500","risk-free") )

##Boxplots rescaling: standardized by subtracting the median and dividing by MAD ##

x1 = SPreturn
x1s = (x1-median(x1))/mad(x1)

x2 = diffrf
x2s = (x2-median(x2))/mad(x2)

boxplot(list(x1s,x2s),boxwex=.5,
        names=list("S&P 500", "risk-free") )


