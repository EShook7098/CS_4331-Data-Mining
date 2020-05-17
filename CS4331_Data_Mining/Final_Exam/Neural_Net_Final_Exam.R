#Show how you would train, test, evaluate, and interpret a neural network in R using the bank-marketing_training and bank_marketing_testing data sets 
#with target "Response" and 3 or 4 continuous and categorical predictors.

bank_train <- read.csv("bank_marketing_training")
bank_test <- read.csv("bank_marketing_test")

#Days since previous will need cleaning
bank_train$days_since_previous <- ifelse(test=bank_train$days_since_previous == 999,
                    yes = NA, no = bank_train$days_since_previous)

bank_test$days_since_previous <- ifelse(test=bank_test$days_since_previous == 999,
                    yes = NA, no = bank_test$days_since_previous)

bank_train$days_since_previous <- as.factor(bank_train$days_since_previous)
bank_test$days_since_previous <- as.factor(bank_test$days_since_previous)
bank_train$housing <- as.factor(bank_train$housing)
bank_test$housing <- as.factor(bank_test$housing)
bank_train$campaign <- as.factor(bank_train$campaign)
bank_test$campaign <- as.factor(bank_test$campaign)
bank_train$emp.var.rate <- as.factor(bank_train$emp.var.rate)
bank_test$emp.var.rate <- as.factor(bank_test$emp.var.rate)
bank_train$age.mm <- (bank_train$age - min(bank_train$age))/(max(bank_train$age)-min(bank_train$age))
bank_test$age.mm <- (bank_test$age - min(bank_test$age))/(max(bank_test$age)-min(bank_test$age))
summary(bank_train)



#install.packages("nnet")
#install.packages("NeuralNetTools")
library(nnet)
library(NeuralNetTools)
#Changing our hidden layer is not seeming to have much effect, possibily choosing poor predictors
#If we use days since previous and too many hidden layer nodes, the net memorizes the data and attempts to label most entries as NA
#Using fewer nodes it does so anyways. Not a great metric for our net - Results in terrible accuracy
nnet01 <- nnet(response~housing+emp.var.rate+campaign+age, data = bank_train, size = 2)
#Acheived convergence
plotnet(nnet01)

#Get the weights
nnet01$wts

ypred <- predict(nnet01, bank_train, type = "class")
t.preds <- table(bank_train$response, ypred)
colnames(t.preds) <- c("Predicted: 0","Predicted: 1")
rownames(t.preds) <- c("Actual: 0", "Actual: 1")
t.preds <- addmargins(A=t.preds, FUN=list(Total = sum), quiet = TRUE)
t.preds


#Evaluation
#Baseline accuracy.
b_acc <- (23872/26841)
b_acc
#First attempt was worse than the baseline model
#Other attempts barely beat the baseline
#2 nodes gives us the best accuracy with these predictors, however our model is not very good
accuracy <- (t.preds[1,1] + t.preds[2,2])/t.preds[3,3]
accuracy

error_rate <- 1-accuracy

sensitivity <- t.preds[2,2]/t.preds[2,3]

specifity <- t.preds[1,1]/t.preds[1,3]

precision <- t.preds[2,2]/t.preds[3,2]
#High sensitivity versus low specificity

recall <- sensitivity

f1 <- 2*(precision * recall)/(precision + recall)

f2 <- 5*(precision * recall)/(4*precision+recall)

f0_5 <- 1.25*(precision*recall)/(0.25*precision+recall)
