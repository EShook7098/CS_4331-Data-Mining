#Show how PCA is performed, evaluated, and interpreted with R or Python using the red_wine_PCA_training and red_wine_PCA_test data sets with target "quality" and predictors, "alcohol", "residual sugar", "pH", "density", and "fixed acidity".

#Show that the PCA components are not correlated and can be used with multiple regression.

#Goals: identify multicollinearity and remove it from our modeling, and reduce dimensionality
wine_test <- read.csv("red_wine_PCA_test")
wine_train <- read.csv("red_wine_PCA_training")

summary(wine_train)
summary(wine_test)

#Seperate predictors and target variable
y <- wine_train$quality
x <- wine_train[,c(2,3,4,5,7)]

x_test <- wine_test[,c(2,3,4,5,7)]
#Normalization
x_z <- as.data.frame(scale(x))
colnames(x_z) <- c("Alcohol_Z", "Residual_Sugar_Z", "pH_Z", "Density_Z", "Fixed_Acidity_Z")

#Correlation Matrix
round(cor(x_z),3)
#We see substantial correlation between some of our predictors, such as with sugar and alcohol

#VIF Values
#install.packages("car")
library(car)
model01 <- lm(formula = y ~ Alcohol_Z + Residual_Sugar_Z + pH_Z + Density_Z + Fixed_Acidity_Z, data = x_z)
vif(model01)
#We see a substantial VIF In Residual Sugar and pH as they are >5, indicating moderate multicollinearity

#install.packages("psych")
library("psych")

pca01 <- principal(r=x_z, rotate="varimax",nfactors=5)
#Varimax rotation allows for more interpretable results
print(pca01$loadings,cutoff = 0.49)
#We can see that Alcohol, Sugar, pH, and density account for 98% of our total variability. 
#Fixed Acidity does not seem to be a very useful metric, and could be cut regardless of
#multicollinearity to reduce our dimensionality.

#We can see the eigenvalue criterion of RC5 is much less than one, and can recommend
#removing it as such.
#Making sure with test data

x_z_test <- as.data.frame(scale(x_test))
colnames(x_z_test) <- c("Alcohol_Z", "Residual_Sugar_Z", "pH_Z", "Density_Z", "Fixed_Acidity_Z")

#Correlation Matrix
round(cor(x_z_test),3)
#We see substantial correlation between some of our predictors, such as with sugar and alcohol

pca01_test <- principal(r=x_z, rotate="varimax",nfactors=5)
#Varimax rotation allows for more interpretable results
print(pca01$loadings,cutoff = 0.49)
#Similar results
#Lets run PCA without Fixed Acidity

pca02 <- principal(r=x_z, rotate="varimax",nfactors=4)
#Without rotation
pca02_norot <- principal(r=x_z,rotate="none",nfactors=4)
#Test data
pca02_test <- principal(r=x_z_test, rotate="varimax",nfactors=4)
pca02$loadings
pca02_norot$loadings
pca02_test$loadings

#Assuring our results are uncorrelated with VIF
PC1 <- pca02$scores[,1]
PC2 <- pca02$scores[,2]
PC3 <- pca02$scores[,3]
PC4 <- pca02$scores[,4]

#We create a multiple regression model and interpret the VIF scores upon it
pca03_withpcacomp <- lm(formula=y~PC1+PC2+PC3+PC4)
vif(pca03_withpcacomp)
#All VIF scores equate to one, which is the minimum value and mean our predictors are uncorrelated




