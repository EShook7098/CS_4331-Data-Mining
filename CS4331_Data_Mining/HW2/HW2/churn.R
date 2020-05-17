 churn <- read.csv("churn.csv")
set.seed(9)
n <- dim(churn)[1]
churn$Index <- c(1:n)
head(churn)

table(churn$Churn) 
#In the original data set we have:
#2850 False
#483 True
#This is a ratio of nearly 6-1, or 14.5% true.

#Increasing incidence of true Churn variables to 20%
x <-(.2*3333-483)/.8
to.resample <- which(churn$Churn=="True")
our.resample <- sample(x=to.resample, size=230, replace=TRUE)
our.resample.churn <- churn[our.resample,]
churn_rebal <- rbind(churn, our.resample.churn)
temp1 <- table(churn_rebal$Churn)
temp2 <- rbind(temp1, round(prop.table(temp1),4))
#Churn: False - 79.99%, True - 20.01% | Close enough
 
#2
#Data Partitioning
train_split <- runif(n) < .67
churn_train <- churn[train_split,]
churn_test <- churn[!train_split,]

train.n <- dim(churn_train)[1]
test.n <- dim(churn_test)[1]

barplot(c(n, train.n, test.n), 
        xlab="Data Sets",
        ylab="Count (Size)",
        names.arg=c("Total","Training","Testing"),
        col="darkred")
#T-Test for 
t.test(churn_train$Day.Mins, churn_test$Day.Mins)
#Sample means are very similar, p-value > 0.1
#Therefore evidence to reject nullifiable hypothesis is not significant

#Two-Sample Z-Test for
p1 <- sum(churn_train$Churn=="True")/train.n
p2 <- sum(churn_test$Churn=="True")/test.n
p_summed <- (sum(churn_train$Churn=="True") +sum(churn_test$Churn=="True"))/(train.n + test.n)
z.value <- (p1-p2)/sqrt(p_summed*(1-p_summed)*(1/train.n + 1/test.n))
p.value <- pnorm(z.value)
#(Z > Z-value) therefore lower.tail = False
p.value
#Our p-value is large, therefore there is no reason to reject null hypothesis

#Let's model

library(rpart);library(rpart.plot)
library(C50)
#CART Model 1 ------------------------------------------------------------------

tree01 <- rpart(Churn ~ Day.Calls + Day.Mins, data=churn_train,method="class")
rpart.plot(tree01, type = 4, extra = 2)

X <- data.frame(Day.Mins=churn_train$Day.Mins,Day.Calls=churn_train$Day.Calls)
predChurnTree01 <- predict(object=tree01, newdata=X, type="class") #
predChurnTree01_df <- data.frame(Churn = predChurnTree01)
missed <- churn_train[which(churn_train$Churn != predChurnTree01_df$Churn),]
temp1 <- table(churn_train$Churn, predChurnTree01) #Easy to read table of how many were missed
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1

accuracy <- 1-dim(missed)[1]/train.n #This model is pretty accurate at 88%
accuracy

#If customers log more than 264 minutes they are particularly likely to churn. Higher usage charged too much? Losing customers
#All that log more than 317 minutes churn.
#Most of the people that churn are between 264-317 minutes in the day.

X <- data.frame(Day.Mins=churn_test$Day.Mins,Day.Calls=churn_test$Day.Calls)
predChurnTree01 <- predict(object=tree01, newdata=X, type="class")
predChurnTree01_df <- data.frame(Churn = predChurnTree01)
missed <- churn_test[which(churn_test$Churn != predChurnTree01_df$Churn),]
temp1 <- table(churn_test$Churn, predChurnTree01)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1

accuracy <- 1-dim(missed)[1]/train.n
accuracy #Even better, 92% accurate in our test set

#--------------------------------------------------------------------

#This just doesn't work with Rpart- Can't build the confusion table shown in lecture ----------------------------------
#tree01.1 <- rpart(Churn ~ Day.Calls + Day.Mins, data=churn_train,method="class")
#X <- subset(x=churn_test, select=c("Day.Calls","Day.Mins"))
#tree01.1pred <- predict(object=tree01.1,newdata=X)
#temp1 <- table(churn_test$Churn,tree01.1pred) 
#row.names(temp1) <- c("Actual: 0","Actual: 1")
#colnames(temp1) <- c("Predicted:0","Predicted: 1")
# <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
#temp1
#---------------------------------------------------------------------


#This doesn't really find anything significant, except maybe those with 5+ customer service calls are 66% likely to Churn
#tree02 <- rpart(Churn ~ CustServ.Calls, data=churn_train, method="class")
#rpart.plot(tree02,type=4,extra=2)

#Model 2 CART Best Model---------------------------------------------------------

churn_train$Intl.Plan <- factor(churn_train$Intl.Plan)
churn_test$Intl.Plan <- factor(churn_test$Intl.Plan)

tree02 <- rpart(Churn ~ Intl.Plan  + Intl.Calls + Intl.Mins, data=churn_train, method="class")
rpart.plot(tree02,type=4,extra=2)

X <- data.frame(Intl.Plan=churn_train$Intl.Plan,Intl.Calls=churn_train$Intl.Calls,Intl.Mins=churn_train$Intl.Mins)
predChurnTree02 <- predict(object=tree02, newdata=X, type="class")
predChurnTree02_df <- data.frame(Churn = predChurnTree02)
missed <- churn_train[which(churn_train$Churn != predChurnTree02_df$Churn),]
temp1 <- table(churn_train$Churn, predChurnTree02) #Easy to read table of how many were missed. Confusion table
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1 # No false positives. Certainly interesting

accuracy <- (temp1[1,1] + temp1[2,2]) / temp1[3,3] #Both of these lines of code do the same thing
accuracy <- 1-dim(missed)[1]/train.n #This model is pretty accurate at 88%
accuracy

#If customer have the international plan and make less than 3 international calls. They are 100% likely to Churn. 
#This seems extremely significant.
#Nearly half of those that churn have an international plan. 29% that have the plan and make 3 or more international calls are likely to churn


X <- data.frame(Intl.Plan=churn_test$Intl.Plan,Intl.Calls=churn_test$Intl.Calls, Intl.Mins=churn_test$Intl.Mins)
predChurnTree02 <- predict(object=tree02, newdata=X, type="class")

predChurnTree02_df <- data.frame(Churn = predChurnTree02)
missed <- churn_test[which(churn_test$Churn != predChurnTree02_df$Churn),]
temp1 <- table(churn_test$Churn,predChurnTree02)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1 #Interestingly, there were no false positives in this model, though there were far more false negatives than correct positives.

accuracy <- 1-dim(missed)[1]/train.n
accuracy #Accuracy is higher in test data set at nearly 95% 

#Model 3 CART ----------------------------------------------------------

tree03 <- rpart(Churn ~ CustServ.Calls, data=churn_train, method="class")
rpart.plot(tree03, type=4, extra=2)

X <- data.frame(CustServ.Calls=churn_train$CustServ.Calls)
predChurnTree03 <- predict(object=tree03, newdata=X, type="class")
predChurnTree03_df <- data.frame(Churn = predChurnTree03)
missed <- churn_train[which(churn_train$Churn != predChurnTree03_df$Churn),]
temp1 <- table(churn_train$Churn, predChurnTree03)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1 #Has the same issues as the second model, shown by the confusion table

accuracy <- 1-dim(missed)[1]/train.n #This model is pretty accurate at 86%
accuracy
#The more customer service calls logged the more likely they are to churn. Above 5 calls 50% likely to churn

X <- data.frame(CustServ.Calls=churn_test$CustServ.Calls)
predChurnTree03 <- predict(object=tree03, newdata=X, type="class")
predChurnTree03_df <- data.frame(Churn = predChurnTree03)
missed <- churn_test[which(churn_test$Churn != predChurnTree03_df$Churn),]
temp1 <- table(churn_test$Churn, predChurnTree03)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1

accuracy <- 1-dim(missed)[1]/train.n
accuracy  #Accuracy is very good at 93% in test data


#Model 1 C5.0 -----------------------------------------------------------
library(C50)
churn_train$VMail.Plan <- factor(churn_train$VMail.Plan)
churn_test$VMail.Plan <- factor(churn_test$VMail.Plan)
C5.1 <- C5.0(Churn ~ VMail.Plan + Intl.Plan + Day.Mins, data=churn_train, control=C5.0Control(minCases=5))
plot(C5.1)

X <- subset(x=churn_test, select=c("VMail.Plan","Intl.Plan","Day.Mins"))
ypred <- predict (object=C5.1,X)
temp1 <- table(churn_test$Churn,ypred)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1

accuracy <- (temp1[1,1] + temp2[2,2]) / temp1[3,3]
accuracy #This looks good. 82% accuracy. It could certainly be better

sensitivity <- temp1[2,2]/temp1[2,3] 
sensitivity 

#Model 2 C5.0 -----------------------------------------------------------------
#Comparing the best model in CART to C5.0. It is a fair bit worse when applied to the test data
C5.2 <- C5.0(Churn ~ Intl.Plan + Intl.Mins + Intl.Calls, data=churn_train, control=C5.0Control(minCases=5))
plot(C5.2)

X <- subset(x=churn_test, select=c("Intl.Plan","Intl.Mins","Intl.Calls"))
ypred <- predict (object=C5.2,X)
temp1 <- table(churn_test$Churn,ypred)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1

accuracy <- (temp1[1,1] + temp2[2,2]) / temp1[3,3]
accuracy #This looks good. 85% accuracy. 

sensitivity <- temp1[2,2]/temp1[2,3] 
sensitivity #Our sensitivity is higher than the last model

#Model 3 C5.0 ------------------------------------------------------------------

C5.3 <- C5.0(Churn ~ Day.Mins + Night.Mins + Eve.Mins, data=churn_train, control=C5.0Control(minCases=5))
plot(C5.3)

X <- subset(x=churn_test, select=c("Day.Mins","Night.Mins","Eve.Mins"))
ypred <- predict (object=C5.3,X)
temp1 <- table(churn_test$Churn,ypred)
row.names(temp1) <- c("Actual: 0","Actual: 1")
colnames(temp1) <- c("Predicted:0","Predicted: 1")
temp1 <- addmargins (A = temp1, FUN=list(Total = sum), quiet = TRUE)
temp1

accuracy <- (temp1[1,1] + temp2[2,2]) / temp1[3,3]
accuracy #Okay model, 83% accuracy

sensitivity <- temp1[2,2]/temp1[2,3] 
sensitivity  #Highest sensitivity so far

#Overall the C5.0 Models seem to be less accurate than the CART models, though not significantly so.
