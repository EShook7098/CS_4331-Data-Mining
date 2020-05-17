#Show how you would perform, test, evaluate, and interpret k-means clustering in Python or R using the Framingham_Training and Framingham_Test data sets 
#with attributes "Sex" and "Age".

fram_train <- read.csv("Framingham_Training")
fram_test <- read.csv("Framingham_Test")

x <- subset(fram_train,select=c("Sex","Age"))

x_scaled <- as.data.frame(scale(x))
colnames(x_scaled) <- c("Sex", "Age")

library("stats")
#We run k-means clustering and factor the result
kmeans01 <- kmeans(x_scaled, centers = 2)
cluster <- as.factor(kmeans01$cluster)

#We have made our initial clusters, therefore  we separate the values belonging
#To each cluster into two variables to represent each
Cluster1 <- x_scaled[which(cluster == 1),]
Cluster2 <- x_scaled[which(cluster == 2),]

#Now we run k-means clustering on our test set
x_test <- subset(fram_test,select=c("Sex","Age"))

x_scaled_test <- as.data.frame(scale(x_test))

colnames(x_scaled_test) <- c("Sex", "Age")

kmeans01_test <- kmeans(x_scaled_test, centers = 2)
cluster <- as.factor(kmeans01_test$cluster)

#We have made our initial clusters, therefore  we separate the values belonging
#To each cluster into two variables to represent each
Cluster1_test <- x_scaled_test[which(cluster == 1),]
Cluster2_test <- x_scaled_test[which(cluster == 2),]


#We run summaries to interpret our results
summary(Cluster1)
summary(Cluster2)


summary(Cluster1_test)
summary(Cluster2_test)
#Our clusters seem to have changed, which is not entirely unexpected
#In the test set Sex has not seemed to seperate well, or at all, with data from both sexes in each cluster
#However in the training set, the clusters seem to have seperated out the data much better in the test runs
#held on my PC. However, these may change when run by the grader.
