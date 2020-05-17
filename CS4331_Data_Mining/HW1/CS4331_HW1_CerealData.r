CerealData <- read.csv("cereals.csv", fileEncoding="UTF-8-BOM")

#Contingency table
table.v1 <- table(CerealData$Manuf, CerealData$Type)
table.v1
table.v2 <- addmargins(A = table.v1, quiet = TRUE)
table.v2

library(ggplot2)

#Here is a simplifed usage over the lecture notes
#Histogram: Calories w/ Manuf overlay
#Most cereals are between 100-110 calories, with the majority of these being of manuf G and K. In this range are the only cereals of type A
ggplot(CerealData, aes(Calories)) + 
  geom_histogram() +
  geom_histogram(aes(fill = Manuf)) +
  coord_flip() +
  labs(title = "Distribution of Manuf by Calorie Count",
       x = "Calories (Kcal)",
       y = "# of Cereals",
       fill = "Manuf Type")

#Lecture Notes fill: Can assign to the same variable without entirely overwriting, only amendment
#g <- ggplot(CerealData, aes(Calories)) +
  #geom_histogram(aes(fill = Manuf)) +
  #coord_flip() 

#Normalized Histogram: Calorie w/ Manuf overlay
#All cereals of 151-160 calories are of manuf K, while all of 141-150 calories are of manuf R.
#However we know this is a minority of the cereals offered. 
#Cereals of Manuf type G seems to be limited to the upper half of the spectrum
#Cereals of type N seem to be limited to the lower half
ggplot(CerealData, aes(Calories)) +
  geom_histogram(aes(fill = Manuf), position = "fill") +
  coord_flip() +
  labs(title = "Distribution of Manuf by Calorie Count",
       x = "Calories (Kcal)",
       y = "Ratio of Cereals  (% by Manuf Count)",
       fill = "Manuf Type")

#Bar Plot: Manuf w/ Type overlay
#Almost all cereals are of type H, with no discernable reason to the distribution to type H
ggplot(CerealData, aes(Manuf)) +
  geom_bar() + 
  geom_bar(aes(fill = Type)) +
  coord_flip() +
  labs(title = "Distribution of Type by Manuf",
       y = "Count (Type)",
       x = "Manuf")

#Normalized Bar Plot: Manuf w/ Type overlay
#Manuf A only consists of type H cereal
#Manuf G, K, P, and R are only of type C. Manuf N and Q are mixed with a supermajority of type C
ggplot(CerealData, aes(Manuf)) +
  geom_bar(aes(fill = Type), position = "fill") +
  coord_flip() +
  labs(title = "Normalized Distribution of Type by Manuf",
       x = "(Manuf)",
       y= "Type Count (%)")

#We want to bin our calorie counts now for a simplified graph
CerealData$Calories_binned <- cut(x = CerealData$Calories, 
                                  breaks = c(0,90,110,200), 
                                  right = FALSE,
                                  labels = c("Under 90", "91 to 110", "110 and Over"))

#Bar Plot: Binned Calories w/ Manuf overlay
#From this we can tell a majority of cereals have more than 110 calories per serving, while very few have under 90. 
#Most cereals are of type K and G, with these accounting for at least half of the cereals at 91 calories and above.
ggplot(CerealData, aes(Calories_binned)) +
  geom_bar() +
  geom_bar(aes(fill= Manuf)) +
  coord_flip() +
  labs(title = "Distribution of Calories by Manuf",
       x = "Count (Manuf)",
       y = "Cereal Count (Type)",
       fill = "Manuf Type")

#Normalized Bar Plot: Binned Calories w/ Manuf overlay
#From this we  can determine cereals of type G tend to have more calories, while cereals of type N seem to have fewer
ggplot(CerealData, aes(Calories_binned)) +
  geom_bar(aes(fill = Manuf), position = "fill") +
  coord_flip() +
  labs(title = "Normalized Distribution of Calories by Manuf",
       x = "Count (Manuf)",
       y = "Ratio of Cereal Calories (% by Manuf Count)",
       fill = "Manuf Type")
       

