library(tidyverse)
library(ggplot2)
library(tidyverse)
library(dslabs)
library(ggthemes)
library(readr)
#Load data set in
NFL_QB_Data <- read.csv("NFL_QB_Data.csv")
#Preview Data set
head(NFL_QB_Data)
#Rename and change to data frame
NFL <- as.data.frame(NFL_QB_Data)
#Look at correlation in data
NFL_Table <- round(cor(NFL),2)
NFL_Table <- as.table(NFL_Table)
NFL_Table
#Age is the closet correlated to salary 

#Creates model of Salary to Age
NFL_Model <- lm(NFL$Age~NFL$Sal_M)
summary(NFL_Model)
#Line of best fit: Salary = Age(.4153) + 23.0173
#RMSE:0.4437

NFL_Viz <- NFL %>% ggplot()
NFL_Viz + geom_point(aes(Sal_M,Age),col="orange") +
  geom_abline(slope = .4153,intercept = 23.0173,col = "darkblue") +
  labs(title="Linear Model: Salary as a function of Age",
       x = "Salary in Millions",y="Age") + theme_few()

#Create Model of Passer Rating
Pass_Model <- lm(NFL$Age~NFL$Passer.Rating)
summary(Pass_Model)
#Line of Best Fit: Passer Rating = .15(Age) + 14.6
#RMSE: .08

#Create Passer Rating viz
Pass_Viz <- NFL %>% ggplot()
Pass_Viz + geom_point(aes(Passer.Rating,Age),col="orange") +
  geom_abline(slope = .15,intercept = 14.6,col = "darkblue") +
  labs(title="Linear Model: Passer Rating as a function of Age",
       x = "Passer Rating",y="Age") + theme_few()

