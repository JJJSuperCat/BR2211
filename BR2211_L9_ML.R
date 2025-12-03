#BR2211 L8

# Remove the full list of R objects in session
rm(list = ls())
# Set data directory
setwd("C:/Users/jaden/Dropbox/Workfiles/NTU-TEACH/BR2211 - Jade/Lecture Slides/Lecture Data")


# R-example1 --------------------------------------------------------------

## Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)
#applicant's checking and savings account balance are recorded as categorical variables

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(123) # use set.seed to use the same random number sequence as the tutorial
train_sample <- sample(1000, 900)

str(train_sample)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)
plot(credit_model)
# interpreting a few initial branches of the tree results
# Sometimes a tree results in decisions that make little logical sense.
# important to investigate such strange decisions to see whether the tree's logic makes sense for business use.


#Evaluation on training data section:
# 133 out of 900 are wrongly predicted by the model (14.8% error rate)
# 35 are classified as yes but is actually a "no"--> false positives
# 98 are classified as no but is actually a "yes"--> false negatives


## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Evaluation:
# Much better perforMance in general
# 13 (out of 33, 39%) defaulted persons are classified as no --> high false negatives

## Making some mistakes more costly than others

# create dimensions for a cost matrix
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))



# R-example2 --------------------------------------------------------------


## Finding Teen Market Segments ----

## Step 2: Exploring and preparing the data ----
teens <- read.csv("snsdata.csv", stringsAsFactors = TRUE)
str(teens)

# look at missing data for female variable
table(teens$gender)
table(teens$gender, useNA = "ifany")

# look at missing data for age variable
summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

summary(teens$age)

# reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# check our recoding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# finding the mean age by cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) # works

# age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# create a vector with the average age for each gradyear, repeated by person
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))


teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# check the summary results to ensure missing values are eliminated
summary(teens$age)

## Step 3: Training a model on the data ----

# create a z-score standardized data frame for easier interpretation
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

# compare the data before and after the transformation
summary(interests$basketball)
summary(interests_z$basketball)

# create the clusters using k-means
RNGversion("3.5.2") # use an older random number generator
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)


## Step 4: Evaluating model performance ----
# look at the size of the clusters
teen_clusters$size

#5 clusters as pre-determined
# goup sizes are quite different--> maybe a problem maybe not
# extremely small group size may occur, could be that an initial center is chosen at an outlier

# look at the cluster centers
teen_clusters$centers


## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
teens$cluster <- teen_clusters$cluster

# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]

# mean age by cluster
aggregate(data = teens, age ~ cluster, mean)

# proportion of females by cluster
aggregate(data = teens, female ~ cluster, mean)

# mean number of friends by cluster
aggregate(data = teens, friends ~ cluster, mean)
