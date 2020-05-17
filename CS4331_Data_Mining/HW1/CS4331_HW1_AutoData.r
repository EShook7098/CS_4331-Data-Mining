#MPG - Ratio
#Cylinders - Ratio
#Displacement - Ratio
#Horsepower - Ratio
#Weight - Ratio
#Acceleration - Ratio
#Model Year - Ordinal
#Origin - Nominal
#Car Name - Nominal


#Loading in our data and ensuring it has loaded properly | 13-16
AutoData <- read.csv("Auto_mpg_raw.csv", fileEncoding="UTF-8-BOM") #fileEncoding specification removes translation issues in the column name
n <- dim(AutoData)[1]
AutoData$Index <- c(1:n)
head(AutoData)


#boxplot(AutoData[,1]) Clearly we have an extreme outlier
#Original histograms for adjusted data | 22-23
#Only MPG and Horsepower need adjustment
hist(AutoData[,1], xlab="Miles per Gallon", ylab = "# of Cars", main="Miles per Gallon")
hist(AutoData[,4], xlim=c(0,250), xlab="Horsepower", ylab="# of Cars", main="Horsepower")

#Removing misleading Data. MPG of 1000, and Horsepower of 0 | 26-29
AutoData$Miles.per.gallon <- ifelse(test=AutoData$Miles.per.gallon == 1000, 
                                       yes = NA, no = AutoData$Miles.per.gallon)
AutoData$Horsepower<- ifelse(test=AutoData$Horsepower == 0, 
                             yes = NA, no = AutoData$Horsepower)

#Now we can make histograms | 35-58
#Histograms are saved in a folder w/ this project

#From here we can tell most cars have between 12 and 38 miles per gallon. Very few cars have gas mileage above or below this.
#A fair performance is about 25 MPG
hist(AutoData[,1], xlim=c(0,50), nclass = 20, main = "Miles Per Gallon", xlab="Miles per Gallon", ylab="# of Cars")

#Almost all cars have an even number of cylinders along 4, 6, and 8. Though a few cars have 3 and 5 cylinders respectively.
hist(AutoData[,2], main = "Cylinders", xlab="# of Cylinders", ylab="# of Cars")

#Although many cars do have higher displacement, the standard displacement seems to be centered around 80-150 cubed inches
hist(AutoData[,3], xlim=c(0,500), ylim=c(0,100), nclass = 25, main="Displacement", xlab="Displacement (inches^3)", ylab="# of Cars")

#A car with about 80 horsepower seems standard, however many cars outperform this measurement, with some having 3x as much horsepower 
hist(AutoData[,4], xlim=c(0,250), ylim=c(0,80), nclass = 25, main="Horsepower", xlab="Horsepower", ylab="# of Cars")

#Weight seems to be spread between 1800 and 5000 pounds, albeit with a visible right skew bell curve 
hist(AutoData[,5], xlim=c(1000,5500), ylim=c(0,50), nclass=25, main="Weight", xlab="Weight (lbs)", ylab="# of Cars")

#The acceleration of the cars seems to be a standard bell curve, with the average to be 14-15 m/s^2
hist(AutoData[,6], xlim=c(5,25), ylim=c(0,70), nclass = 15, main="Acceleration", xlab="Acceleration (m/s^2)", ylab="# of Cars")

#Almost all cars on the list are old models, being produced between 70' and 82'. With the exception of more cars being produced
#in 70', there is an even distribution across the remaining years
hist(AutoData[,7], main="Year Produced", xlab="Year", ylab="# of Cars")

#Most cars on this list are from origin 1, with the remaining cars split about evenly between origin 2 and 3
hist(AutoData[,8], main="Origin", xlab="Origin", ylab="# of Cars")

#Z values found and outliers generated for numeric data |  61-77
AutoData$Miles.per.gallon_z <- scale(x=AutoData$Miles.per.gallon)
AutoData_MPG_Outliers <- subset(AutoData, AutoData$Miles.per.gallon_z > 3 | AutoData$Miles.per.gallon_z < -3)

AutoData$Displacement_z <- scale(x=AutoData$Displacement)
AutoData_Displacement_Outliers <- subset(AutoData, AutoData$Displacement_z > 3 | AutoData$Displacement_z < -3)

AutoData$Horsepower_z <- scale(x=AutoData$Horsepower)
AutoData_Horsepower_Outliers <- subset(AutoData, AutoData$Horsepower_z > 3 | AutoData$Horsepower_z < -3)

AutoData$Weight_z <- scale(x=AutoData$Weight)
AutoData_Weight_Outliers <- subset(AutoData, AutoData$Weight_z > 3 | AutoData$Weight_z < -3)

AutoData$Acceleration_z <- scale(x=AutoData$Acceleration)
AutoData_Acceleration_Outliers <- subset(AutoData, AutoData$Acceleration_z > 3 | AutoData$Acceleration_z < -3)
#This may not be needed but the data is numeric regardless
AutoData$Cylinders_z <- scale(x=AutoData$Cylinders)
AutoData_Cylinders_Outliers <- subset(AutoData, AutoData$Cylinders_z > 3 | AutoData$Cylinders_z < -3)

#Only Acceleration and MPG have outliers | 82-83
#Acceleration outliers are on the high end of the spectrum however their Horsepower is on the lower end, they are both of European make
#The cars with the highest Horsepower though are some of the heaviest cars with the most displacement and are all of North American make
AutoData_Acceleration_Outliers
AutoData_Horsepower_Outliers

#Write out our adjusted data frame | 86
write.csv(AutoData, "Auto_mpg_adjust.csv")
