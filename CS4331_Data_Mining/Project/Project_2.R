library(plyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(C50)
# 
# LoanNr_ChkDgt : Identifier - Primary key                 | Integer | Nominal 
#   
# Name          : Borrower name                            | String | Nominal
# 
# City          : Borrower city                            | String | Nominal
# 
# State         : Borrower state                           | String | Nominal | Categorical
# 
# Zip           : Borrower zip code                        | Integer | Nominal
#     
# Bank          : Bank name                                | String |Nominal  | Categorical
# 
# BankState     : Bank state                               | String | Nominal | Categorical
# 
# NAICS         : North American industry classification system code | Integer | Nominal
# 
# ApprovalDate  : Date SBA commitment issued               | Date | Ordinal   | Numerical
# 
# ApprovalFY    : Fiscal year of commitment                | DateYear | Ordinal | Numerical #
# Most variables before here are likely not very interesting, or will entail a far too in depth model later (Such as state, if it branches over 50 new nodes)
#
# Term          : Loan term (months)                       | Integer | Ratio - Discrete | Numerical #
# 
# NoEmp         : Number of business employees             | Integer | Ratio - Discrete | Numerical #
# 
# NewExist      : 1 = Existing business, 2 = New business  | Integer | Nominal | Categorical
#  
# CreateJob     : Number of jobs created                   | Integer | Ratio - Discrete | Numerical #
# 
# RetainedJob   : Number of jobs retained                  | Integer | Ratio - Discrete | Numerical #
# 
# FranchiseCode : Franchise code, (00000 or 00001) = No franchise | Integer | Nominal   | Categorical - Convert numerical. 
#                 Conversion - 0 = No Franchise, 1 = Franchise
# 
# UrbanRural    : 1 = Urban, 2 = rural, 0 = undefined       | Integer | Nominal | Categorical
# 
# RevLineCr     : Revolving line of credit: Y = Yes, N = No | Boolean | Nominal | Categorical
# 
# LowDoc        : LowDoc Loan Program: Y = Yes, N = No      | Boolean | Nominal | Categorical 
# 
# ChgOffDate    : The date when a loan is declared to be in default | Date | Ordinal | Numerical 
# 
# DisbursementDate : Disbursement date                      | Date | Ordinal | Numerical # 
# 
# DisbursementGross : Amount disbursed                      | Integer | Ratio - Continuous | Numerical * #
# 
# BalanceGross  : Gross amount outstanding                  | Integer | Ratio - Continuous | Numerical * #
#   
# MIS_Status    : Loan status charged off = CHGOFF, Paid in full = PIF | Nominal | Categorical *
#  
# ChgOffPrinGr  : Charged-off amount                        | Integer | Ratio - Continuous | Numerical * #
# 
# GrAppv        : Gross amount of loan approved by bank     | Integer | Ratio - Continuous | Numerical * #
# 
# SBA_Appv      : SBA-s guaranteed amount of approved loan  | Integer | Ratio - Continuous | Numerical * #
#
# * Indicates a later conversion to numerical
# # Indicates a likely need to bin

loan_df <- read.csv("SBAnational.csv")
n <- dim(loan_df)[1]
loan_df$Index <- c(1:n)

summary(loan_df)
head(loan_df)


#--- Cleaning Data and Conversion of Dollar Amounts to Numerical -----
#Cleaning rows that contain $ values to be truly numeric.
#Simple regex expression removes $ and , from US dollar expression and converts to numeric to remove trailing .0*

loan_df$DisbursementGross <- as.numeric(gsub("[$,]", "", loan_df$DisbursementGross))
loan_df$BalanceGross <- as.numeric(gsub("[$,]", "", loan_df$BalanceGross))
loan_df$ChgOffPrinGr <- as.numeric(gsub("[$,]", "", loan_df$ChgOffPrinGr))
loan_df$GrAppv <- as.numeric(gsub("[$,]", "", loan_df$GrAppv))
loan_df$SBA_Appv <- as.numeric(gsub("[$,]", "", loan_df$SBA_Appv))

#Remapped the MIS_Status variable to RiskFactor ~ High/Low thus creating the target Variable
#loan_df$RiskFactor <- revalue(x=loan_df$ChgOffPrinGr, replace = c("0" = "Low", "High"))

loan_df$RiskFactor <- ifelse(test = loan_df$ChgOffPrinGr == 0,
                             yes = "Low", no = "High")

#Removed null values
loan_df$UrbanRural <- ifelse(test= loan_df$UrbanRural == '', 
                                yes = NA, no = loan_df$UrbanRural)


loan_df$RevLineCr <- ifelse(test= loan_df$RevLineCr == '', 
                               yes = NA, no = loan_df$RevLineCr)

#----------- Checking for outliers -----------------------------------
loan_df$Term_z <- scale(x = loan_df$Term)
loan_outliers_Term <- loan_df[which(loan_df$Term_z < -3 | loan_df$Term_z > 3),]
#Outliers past 348 month terms

loan_df$NoEmp_z <- scale(x = loan_df$NoEmp)
loan_outliers_NoEmp <- loan_df[which(loan_df$NoEmp_z < -3 | loan_df$NoEmp_z > 3),]
#Outliers past 248 employees

loan_df$CreateJob_z <- scale(x = loan_df$CreateJob)
loan_outliers_CreateJob <- loan_df[which(loan_df$CreateJob_z < -3 | loan_df$CreateJob_z > 3),]
#Outliers past 750 jobs created

loan_df$RetainedJob_z <- scale(x = loan_df$RetainedJob)
loan_outliers_RetainedJob <- loan_df[which(loan_df$RetainedJob_z < -3 | loan_df$RetainedJob_z > 3),]
#Outliers begin past 750 jobs retained - This and all above <1000 outliers

loan_df$DisbursementGross_z <- scale(x = loan_df$DisbursementGross)
loan_outliers_DisbursementGross <- loan_df[which(loan_df$DisbursementGross_z < -3 | loan_df$DisbursementGross_z > 3),]
#Outliers begin past $1064200 - 19k outliers

loan_df$BalanceGross_z <- scale(x = loan_df$BalanceGross)
loan_outliers_BalanceGross <- loan_df[which(loan_df$BalanceGross_z < -3 | loan_df$BalanceGross_z > 3),]
#Outliers begin past $9111 - Only 12 outliers

loan_df$ChgOffPrinGr_z <- scale(x = loan_df$ChgOffPrinGr)
loan_outliers_ChgOffPrinGr <- loan_df[which(loan_df$ChgOffPrinGr_z < -3 | loan_df$ChgOffPrinGr_z > 3),]
#Outliers begin past $208964 - 13k outliers

loan_df$GrAppv_z <- scale(x = loan_df$GrAppv)
loan_outliers_GrAppv <- loan_df[which(loan_df$GrAppv_z < -3 | loan_df$GrAppv_z > 3),]
#Outliers begin past $1042500 - 19k outliers

loan_df$SBA_Appv_z <- scale(x = loan_df$SBA_Appv)
loan_outliers_SBA_Appv <- loan_df[which(loan_df$SBA_Appv_z < -3 | loan_df$SBA_Appv_z > 3),]
#Outliers begin past $834750 - 18k outliers

#Overall the outliers need no deletion, I think it is for the best that we keep them all within the data set as they seem to not represent
# - misleading data

#--------- Categorical to Numerical Conversions ------------

#We want to populate a new variable that determines if the business is a franchise or not.
#0 indicates no franchise, 1 indicates a franchise
loan_df$FranchiseExist <- ifelse(test = loan_df$FranchiseCode <= 1,
                                 yes = 0, no = 1)

#Looking at potentially interesting variables, seeing proportions. Could use these to plot against other values
NewExist <- addmargins (A = table(loan_df$NewExist), FUN=list(Total = sum), quiet = TRUE) #Most loan seekers possess an existing business
NewExist
#A little under 3:1 businesses are existing to not yet existing. Do existing businesses default less often than new businesses?

UrbanRural <- addmargins (A = table(loan_df$UrbanRural), FUN=list(Total = sum), quiet = TRUE) #Most loan seekers possess an existing business
UrbanRural 
#Roughly 1/3 are undefined. The other 2/3 are split roughly 5:1 into urban:rural. How well do rural loans fare compared to urban?

#Next steps. Create graphs of interesting variables/relations and use the graphs to determine how they should best be binned


#----------------------- Binning ---------------------------------
ggplot(loan_df,aes(NewExist)) + geom_bar() + coord_flip()
ggplot(loan_df, aes(NewExist)) +  geom_bar(aes(fill=RiskFactor)) + coord_flip()
ggplot(loan_df, aes(NewExist)) +  geom_bar(aes(fill=RiskFactor), position="fill") + coord_flip()

# May be a trend for the smaller the loan the more likely to default
loan_df$GrAppv_binned <- cut(x=loan_df$GrAppv,breaks=c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,6000000),right=FALSE, labels=c("Under 100k","100k to 200k","200k to 300k","300k to 400k","400k to 500k","500k to 600k","600k to 700k","700k to 800k","800k to 900k","900k to 1m", "1m+")) 
ggplot(loan_df, aes(GrAppv_binned)) + geom_bar(aes(fill=RiskFactor)) + coord_flip()
ggplot(loan_df, aes(GrAppv_binned)) + geom_bar(aes(fill=RiskFactor), position="fill") + coord_flip() 

loan_df$Term_binned <- cut(x=loan_df$Term,breaks=c(0,30,60,90,120,150,180,220,250,280,300,500),right=FALSE, labels=c("Under 30","30-60","60-90","90-120","120-150","150-180","180-210","210-240","270-300","300", "Over 300")) 
ggplot(loan_df, aes(Term_binned)) + geom_bar()

loan_df$DisbursementGross_binned <- cut(x=loan_df$DisbursementGross, breaks=c(0,20000,42000,73000,100000,150000,175000,200000,225000,275000,350000, 500000, 9999999999),
                                        right = FALSE,
                                        labels =c("0-20000","20000-42000","42000-73000","73000-100000","100000-150000","150000-175000","175000-200000","200000-225000","225000-275000","275000-350000","350000-500000","500000+"))
ggplot(loan_df, aes(DisbursementGross_binned)) + geom_bar()

loan_df$NoEmp_binned <- cut(x=loan_df$NoEmp, breaks=c(0,2,4,6,8,10,15,20,100,1000,10000),
                                        right = FALSE,
                                        labels =c("0-2","3-4","5-6","7-8","9-10","11-15","16-20","21-100","101-1000","1001+"))
ggplot(loan_df, aes(NoEmp_binned)) + geom_bar()


loan_df$SBA_Appv_binned <- cut(x=loan_df$SBA_Appv,breaks=c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,6000000),right=FALSE, labels=c("Under 100k","100k to 200k","200k to 300k","300k to 400k","400k to 500k","500k to 600k","600k to 700k","700k to 800k","800k to 900k","900k to 1m", "1m+")) 
ggplot(loan_df, aes(SBA_Appv_binned)) + geom_bar(aes(fill=RiskFactor)) + coord_flip()
ggplot(loan_df, aes(SBA_Appv_binned)) + geom_bar(aes(fill=RiskFactor), position="fill") + coord_flip()


#---------------- Rebalancing Data for Partition ---------------
# Find how much to rebalance our Data set in order to get a 25% high risk factor
x<-(.25*899164-162012)/.75
to.resample<-which(loan_df$RiskFactor=="High")

# size comes from calculated x
our.resample<-sample(x=to.resample,size=83706,replace=TRUE)
our.resample.records<-loan_df[our.resample,]
loan_rebal<-rbind(loan_df,our.resample.records)
t.v1<-table(loan_rebal$RiskFactor)
t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Low","High")
rownames(t.v2) <- c("Count","Proportion")
t.v2

#------------ Partitioning ------------------------------
# Partitions the data frame  train70:30test
set.seed(42);
n <- dim(loan_rebal)[1]
loan_ind <- runif(n) < 0.70
loan_train <- loan_rebal[loan_ind,]
loan_test <- loan_rebal[!loan_ind,]

#Find proportion of High Risk Factor
n <- dim(loan_train)[1]
t.v1 <- table(loan_train$RiskFactor)
t.v1[1]/n
#25%, acceptable. Slap that like button and subscribe

#-------------- Validation of partition ------------------------
#These tests show that there is no significant variance between the training and test sets, given a high p-value

# We are choosing GrAppv because it has a fairly standardized H/L across all values
t.test(loan_train$GrAppv,loan_test$GrAppv)
#Sample means are very similar, p-value > 0.05
#Therefore evidence to reject nullifiable hypothesis is not significant

p1<-sum(loan_train$RiskFactor=="High")/dim(loan_train)[1]
p2<-sum(loan_test$RiskFactor=="High")/dim(loan_test)[1]
p_pooled<-(sum(loan_train$RiskFactor=="High")+sum(loan_test$RiskFactor=="High"))/(dim(loan_train)[1]+dim(loan_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(loan_train)[1]+1/dim(loan_test)[1]))
p <- pnorm(z)
#(Z > Z-value) therefore lower.tail = False
p

#Our p-value is large, therefore there is no reason to reject null hypothesis
loan_train.n = dim(loan_train)[1] / dim(loan_rebal)[1]
loan_test.n = dim(loan_test)[1] / dim(loan_rebal)[1]


# At this point we ate tacos together over the internet


# Given the way we partitioned our data set a model that assigns low risk to all entries is 75% correct
# This approves all of the loans

#Proper proportions created, shown in a visual format
barplot(c(1, loan_train.n, loan_test.n),
        ylab = "Proportions",
        names.arg = c("Rebal", "Training", "Test"),
        col = "lightblue")


#---------- Factorizing for Models -------------------------------------------
loan_train$RiskFactor <- factor(loan_train$RiskFactor)

#------------------- C50 Models -----------------------------------------------
#C50 Model 1
#This model bins the Term data, therefore making it much faster and easier to read.
#However, it loses approximately ~10% accuracy when applied to the test data
C5 <- C5.0(RiskFactor ~ FranchiseCode + RevLineCr + Term_binned, data = loan_train, control = C5.0Control(minCases=75))
plot(C5)

X = data.frame(FranchiseCode = loan_test$FranchiseCode, RevLineCr = loan_test$RevLineCr, Term_binned = loan_test$Term_binned)

predLoanCART = predict(object = C5, newdata = X)
predLoanCART_df = data.frame(RiskFactor = predLoanCART) 
missed <- loan_train[which(loan_test$RiskFactor != predLoanCART_df$RiskFactor),]
c5Con1 <- table(loan_test$RiskFactor,predLoanCART)
row.names(c5Con1) <- c("Actual: 0:","Actual: 1")
colnames(c5Con1) <- c("Predicted: 0","Predicted: 1")
c5Con1 <- addmargins(A = c5Con1, FUN=list(Total = sum), quiet = TRUE)
c5Con1 

#~84% Accurate
c5accuracy1 <- (c5Con1[1,1] + c5Con1[2,2])/c5Con1[3,3]
c5accuracy1



#C50 Model 2
#This model has a higher degree of accuracy than our first. We used the unbinned values of Term.
#This creates a very atrocious looking model to analyze by sight, but when applied to the data is better, other than
#in terms of speed.
C5 <- C5.0(RiskFactor ~ FranchiseCode + RevLineCr + Term, data = loan_train, control = C5.0Control(minCases=75))
plot(C5)

X = data.frame(FranchiseCode = loan_test$FranchiseCode, RevLineCr = loan_test$RevLineCr, Term = loan_test$Term)

predLoanCART = predict(object = C5, newdata = X)
predLoanCART_df = data.frame(RiskFactor = predLoanCART) 
missed <- loan_train[which(loan_test$RiskFactor != predLoanCART_df$RiskFactor),]
c5Con2 <- table(loan_test$RiskFactor,predLoanCART)
row.names(c5Con2) <- c("Actual: 0:","Actual: 1")
colnames(c5Con2) <- c("Predicted: 0","Predicted: 1")
c5Con2 <- addmargins(A = c5Con2, FUN=list(Total = sum), quiet = TRUE)
c5Con2

#~92% Accurate
c5accuracy2 <- (c5Con2[1,1] + c5Con2[2,2])/c5Con2[3,3]
c5accuracy2



#C50 Model 3
C5 <- C5.0(RiskFactor ~ Term_binned + NoEmp_binned + GrAppv_binned, data = loan_train, control = C5.0Control(minCases=75))
plot(C5)

X = data.frame(Term_binned = loan_test$Term_binned, NoEmp_binned = loan_test$NoEmp_binned, GrAppv_binned = loan_test$GrAppv_binned)

predLoanCART = predict(object = C5, newdata = X)
predLoanCART_df = data.frame(RiskFactor = predLoanCART) 
missed <- loan_train[which(loan_test$RiskFactor != predLoanCART_df$RiskFactor),]
c5Con3 <- table(loan_test$RiskFactor,predLoanCART)
row.names(c5Con3) <- c("Actual: 0:","Actual: 1")
colnames(c5Con3) <- c("Predicted: 0","Predicted: 1")
c5Con3 <- addmargins(A = c5Con3, FUN=list(Total = sum), quiet = TRUE)
c5Con3

# ~84% Accurate
c5accuracy3 <- (c5Con3[1,1] + c5Con3[2,2])/c5Con3[3,3]
c5accuracy3

#Binning Term makes for a significantly more accurate model, however it is horrific to read.
#------------------- CART Models -----------------------------------------------
#CART Model 1
#Unable to create a useful model without Term. How 'bout that
#This creates a model with roughly 90% accuracy
cart01 <- rpart(RiskFactor ~  Term_binned + DisbursementGross_binned, data = loan_train, method = "class",control=rpart.control(minsplit=1000))
rpart.plot(cart01, type = 4, extra = 2)

X = data.frame(DisbursementGross_binned = loan_test$DisbursementGross_binned, Term_binned = loan_test$Term_binned)

predLoanCART = predict(object = cart01, newdata = X, type = "class")
predLoanCART_df = data.frame(RiskFactor = predLoanCART)
missed <- loan_train[which(loan_test$RiskFactor != predLoanCART_df$RiskFactor),]
cartCon1 <- table(loan_test$RiskFactor,predLoanCART)
row.names(cartCon1) <- c("Actual: 0:","Actual: 1")
colnames(cartCon1) <- c("Predicted: 0","Predicted: 1")
cartCon1 <- addmargins(A = cartCon1, FUN=list(Total = sum), quiet = TRUE)
cartCon1

#83% Accurate
cartaccuracy1 <- (cartCon1[1,1] + cartCon1[2,2])/cartCon1[3,3]
cartaccuracy1



#CART Model 2
#A serious question arose with the utter demand of Term existing to make any trees
#How accurate is term alone? The answer will hurt the soul
#Very
cart01 <- rpart(RiskFactor ~  Term, data = loan_train, method = "class",control=rpart.control(minsplit=1000))
rpart.plot(cart01, type = 4, extra = 2)

X = data.frame(FranchiseExist = loan_test$FranchiseExist, Term = loan_test$Term, NoEmp_binned = loan_test$NoEmp_binned)

predLoanCART = predict(object = cart01, newdata = X, type = "class")
predLoanCART_df = data.frame(RiskFactor = predLoanCART)
missed <- loan_train[which(loan_test$RiskFactor != predLoanCART_df$RiskFactor),]
cartCon2 <- table(loan_test$RiskFactor,predLoanCART)
row.names(cartCon2) <- c("Actual: 0:","Actual: 1")
colnames(cartCon2) <- c("Predicted: 0","Predicted: 1")
cartCon2 <- addmargins(A = cartCon2, FUN=list(Total = sum), quiet = TRUE)
cartCon2

cartaccuracy2 <- (cartCon2[1,1] + cartCon2[2,2])/cartCon2[3,3]
#This disappoints me greatly - 90%
cartaccuracy2




#------------ Baseline Model ----------------------
#Baseline of test set
nTest <- dim(loan_test)[1]
baselineLow <- sum(loan_test$RiskFactor == "Low")
baselineHigh <- sum(loan_test$RiskFactor == "High")
baselineAccuracy <- baselineLow/nTest


#------------------- Calculating Model Costs -----------------------------------------------
# We are stating that our average loss for a false positive is the
# Average of all money lost (defaulted amounts) for all loans
FPCost <- sum(loan_df$ChgOffPrinGr) / sum(loan_df$RiskFactor=="High")

# We are stating that our average gain from a true positive is the 
# Average of all interest gained from all PIF loans
loan_df$PIF <- ifelse(test = loan_df$RiskFactor == "Low",
                      yes = loan_df$DisbursementGross, no = 0)

# We are using the average national interst rate for SBA loans 9% as
# our interest rate across all loans
TPGain <- sum(loan_df$PIF) / sum(loan_df$RiskFactor=="Low") * .09

cat("False Positive Cost: ", FPCost)
cat("True Positive Gain: ", TPGain)

#Baseline Model - All Predicted Low Risk

#C5 Model 1
TotalCost1 <- c5Con1[4][1] * FPCost - c5Con1[5][1]*TPGain
TotalCost1
TotalCostAvg1 <- TotalCost1/c5Con1[9][1]
TotalCostAvg1
#Roughly $6100 gained per loan

#C5 Model 2
TotalCost2 <- c5Con2[4][1] * FPCost - c5Con2[5][1]*TPGain
TotalCost2
TotalCostAvg2 <- TotalCost2/c5Con2[9][1]
TotalCostAvg2
#Roughly $10700 gained per loan

#C5 Model 3
TotalCost3 <- c5Con3[4][1] * FPCost - c5Con3[5][1]*TPGain
TotalCost3
TotalCostAvg3 <- TotalCost3/c5Con3[9][1]
TotalCostAvg3
#Roughly 6300 gained per loan

#CART Model 1
TotalCost4 <- cartCon1[4][1] * FPCost - cartCon1[5][1]*TPGain
TotalCost4
TotalCostAvg4 <- TotalCost4/cartCon1[9][1]
TotalCostAvg4
#Roughly $6600 gained per loan

#CART Model 2
TotalCost5 <- cartCon2[4][1] * FPCost - cartCon2[5][1]*TPGain
TotalCost5
TotalCostAvg5 <- TotalCost5/cartCon2[9][1]
TotalCostAvg5
#Roughly 9400 gained per loan

#Baseline
#
TotalCost6 <- baselineHigh * FPCost - baselineLow * TPGain
TotalCost6
TotalCostAvg6 <- TotalCost6/nTest
TotalCostAvg6
#Roughly $4100 lost per loan

#Every model outperforms the baseline significantly - Where the baseline approves every loan
#C50 Model 2 performs the best of them all by a fair margin, where FranchiseCode, RevLineCr, and unbinned Term are used
#to create the model