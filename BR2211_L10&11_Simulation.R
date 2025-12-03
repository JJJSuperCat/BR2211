#BR2211 L9 & 10

# Remove the full list of R objects in session
rm(list = ls())
# Set data directory
setwd("/Users/joeyzhou/Library/CloudStorage/OneDrive-Personal/0.OneDrive_ZhouFiles/A.Work/A.NTU/A.Uni.Courses/BR2211 Financial and Risk Analytics I/2025 Material (Joey)/Lecture Slides/Lecture Data")

###########  R script ####################################################################
###########  of Handbook in Monte Carlo Simulation    ####################################
###########  by Paolo Brandimarte ########################################################



# R-example1 --------------------------------------------------------------

# Accept-rejection method for generating Beta r.v. with both shape parameter equal to 3

f <- function(x) dbeta(x,shape1=3,shape2=3)
out <- optimize(f,interval=c(0,1),maximum=TRUE)
c <- out$objective
x <- seq(from=0,to=1,by=0.01)
plot(x,f(x),type='l')
set.seed(55555)
for (k in 1:1000){
  U1 <- runif (1)
  U2 <- runif(1,min=0,max=c)
  if (U2 <= f(U1))
    points(U1,U2,pch=20)
  else
    points(U1,U2,pch=1)
}




# R-example2 --------------------------------------------------------------

system.time(qnorm(runif(50000000)))
system.time(rnorm(50000000))



# R-example3 --------------------------------------------------------------

#simulating AR(1) process

AR <- function(XO,alpha0,alpha1,sigma,T){
  X <- rep (0,T+1)
  X[1] <- XO
  for (t in 1:T) {
    X[t+1] <- alpha0+alpha1*X[t]+rnorm(1,0,sigma)
  }
  plot(0:T,X,pch=15)
  lines(0:T,X)
}
  
#1. In order to allocate memory to an array, we use rep (0, T+l) here; as an   alternative, we may also use numeric (T+l).
#2. The syntax of for loops is self-explanatory. Braces are used to enclose  multiple statements; in this case, since there is only one statement inside the loop, they could be omitted.
#3. The function plot is used here to lay down dots, with a character specified by the optional pch parameter; the function lines connects the dots with lines, without opening another window.

set.seed(55555)
AR(40,8,0.8,1,50)


# R-example4 --------------------------------------------------------------

#scalar Geometric Brownian Motion (loop version)
simGBM <- function(S0,mu,sigma,T,numSteps,numRepl){
  dt <- T/numSteps
  # pre-compute invariant quantities
  nuT <- (mu-sigma^2/2)*dt
  sigmaT <- sqrt(dt)*sigma
  # allocate matrix to store sample paths
  pathMatrix <- matrix(nrow=numRepl,ncol=numSteps+1)
  pathMatrix[,1] <- S0
  for(i in 1:numRepl)
    for(j in 2:(numSteps+1))
      pathMatrix[i,j] <- pathMatrix[i,j-1]*exp(rnorm(1,nuT,sigmaT))
  return(pathMatrix)
}

# two nested "for" loops, contrary to common recommendations concerning R code efficiency.
#1.	The function creates a matrix of sample paths, where the replications are stored row by row and columns correspond to time instants.
#2.	The first column contains the initial price for all sample paths.
#3.	The input arguments are the initial S0, the drift mu, the volatility sigma, the time horizon T, the number of time steps numSteps, and the number of replications numRepl.

# scalar Geometric Brownian Motions (vectorized version) 
simvGBM <- function (S0,mu,sigma,T,numSteps,numRepl){
  dt <- T/numSteps
  nuT <- (mu-sigma^2/2)*dt 
  sigmaT <- sqrt(dt)*sigma
  normMatrix <- matrix(rnorm(numSteps*numRepl,nuT,sigmaT), nrow=numRepl)
  logincrements <- cbind(log(S0),normMatrix) 
  logPath <- t(apply(logincrements,1,cumsum))
  return(exp(logPath))
}

# converting equation to be ln(d St)=(mu-sigma^2/2) dt + sigma dWt


# simulation with mu=0.1 and sigma=0.1 & 0.4
set.seed(55555)
paths1 <-simGBM(50,0.1,0.1,1,300,5)
paths2 <-simGBM(50,0.1,0.4,1,300,5)
yBounds <- c(min(min(paths1),min(paths2)),max(max(paths1),max(paths2)))
par(mfrow=c(1,2))
plot(1:301,paths1[1,],ylim=yBounds,type="l",xlab="sigma=10%",ylab='')
for(j in 2:5)
  lines(1:301,paths1[j,])
plot(1:301,paths2[1,],ylim=yBounds,type="l",xlab="sigma=40%",ylab='')
for(j in 2:5)
  lines(1:301,paths2[j,])
par(mfrow=c(1,1))



# R-example5 --------------------------------------------------------------


#generalized brownian motion plus jumps

set.seed(55555)

T<- 1;
numSteps <- 1000; 
h<- T/numSteps

#parameters of the jump component
lambda <- 7; 
nu <- 0; 
xi <- 2
#parameters of the diffusion component
mu <- 10; 
sigma <- 2

#sample the diffusion increments
dX <- mu*h+sigma*sqrt(h)*rnorm(numSteps)

#sample and add the jump component
N <- rpois(1,lambda*T)
jumpPos <- ceiling(runif(N)*T*numSteps)
jumpSize<- rnorm(N,nu,xi)
dX[jumpPos] <- dX[jumpPos]+jumpSize

#add components and cumulate sample path
X <- c(0,cumsum(dX))
if(N==0){
  plot(0:numSteps,X,type='l')
} else{
  plot(0:numSteps,X,type='l')
  points(jumpPos,X[jumpPos])
  points(jumpPos,X[jumpPos+1],pch=19)
}



# R-example6 --------------------------------------------------------------


MMl_Queue <- function(lambda, mu, howmany){
  W <- numeric(howmany)
  for (j in 1:howmany){
    intTime <- rexp(1,rate=lambda)
    servTime <- rexp(1,rate=mu)
    W[j] <- max(0, W[j-1]+servTime-intTime)
  }
  return(mean(W))
}

set.seed(55555)

howmany=100000


lambda = 1; mu = 1.1
rho = lambda/mu # utilization is 90.903
rho/mu/(1-rho) # exact value 9.09

MMl_Queue(lambda,mu,howmany)
MMl_Queue(lambda,mu,howmany)
MMl_Queue(lambda,mu,howmany)


# change parameters
lambda = 1; mu = 2
rho = lambda/mu # utilization is 50%
rho/mu/(1-rho) #exact value 0.5

MMl_Queue(lambda,mu,howmany)
MMl_Queue(lambda,mu,howmany)
MMl_Queue(lambda,mu,howmany)



# R-example7 --------------------------------------------------------------


MMl_Queue_V <- function(lambda=1,mu=1.1,howmany=10000){
  W=numeric(howmany)
  for(j in 1:howmany){
    intTime=rexp(1,rate=lambda)
    servTime=rexp(1,rate=mu)
    W[j]=max(0,W[j-1]+servTime-intTime)
  }
  return(W)
}

set.seed(55555)
#check simulated mean
mean(MMl_Queue_V()) #run a few times (7-8times) to check results differences

#check simulated confidence interval
as.numeric(t.test(MMl_Queue_V())$conf.int) #check confidence intervals, run a few times

#check ACF
W1 <- MMl_Queue_V()
W2 <- MMl_Queue_V(lambda=1,mu=2)
par(mfrow=c(1,2))
acf(W1,main="utilization=90.91%")
acf(W2,main="utilization=50%")

par(mfrow=c(1,1))

