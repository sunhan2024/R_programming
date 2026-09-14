setwd("C:/Users/sunha/Desktop/INFO634")
#set the correct working directory to prepare to read csv file
dia <- read.csv("dia.csv")
#read the csv file into a data frame called "dia"
str(dia)
#view the variables and check their class
dia$cut <- factor(dia$cut)
dia$clarity <- factor(dia$clarity)
dia$colour <- factor(dia$colour)
#change the type from character to factor, for a better future analysis
levels(dia$cut)
levels(dia$colour)
levels(dia$clarity)
#view the level of each variable to see if they need to be reordered
dia$colour <- factor(dia$colour, levels = c("J","I","H","G","F","E","D"))
#Since the "lm" model will take the first level as the reference category
#I reordered the "colour" to let the worst colour be the first level, 
#and to be consistent with the other two variables. 
#Then the three categorical variables all have the worst category being the first level
mymodel <- lm(data=dia, price~carat+cut+colour+clarity)
#create a multiple linear regression model of the price
# and assign it to "mymodel"
summary(mymodel)
#view all the coefficients of each categorical variable
mean(diff(c(0,611.84,786.05,826.51,926.02)))
mean(diff(c(0,1046.30,1452.28,1945.22,2079.75,2241,71,2387.33)))
mean(diff(c(0,2218.76,3167.56,3811.46,4155.91,4430.33,4667.13,4767.44)))
#calculate the mean of differences of adjacent coefficients of each variable
library(ggplot2)
ggplot(data = dia, aes(x=price, y=mymodel$residuals))+
  geom_point()
#generate a scatter plot of the residuals versus price
dia$residuals <- mymodel$residuals
ggplot(data = dia, aes(x=residuals))+
  geom_density()
#generate a density plot of the residuals
qqnorm(mymodel$residuals,col="black",pch=2)
qqline(mymodel$residuals,col="red",lwd=1.5)
#generate qqnorm and qqline plots of the residuals
shapiro.test(mymodel$residuals)
#perform shapiro test of the residuals