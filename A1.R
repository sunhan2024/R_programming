# set the working directory
setwd("C:/Users/sunha/Desktop/INFO634")
#read the csv file and assign it to a data frame variable
df_temp <- read.csv("centralparktemps.csv")
#install measurements package
install.packages("measurements")
#library the measurements package
library(measurements)
#figure out how to use conv_unit
?conv_unit
#convert degrees Fahrenheit to Celsius and assign them to a new attribute
df_temp$celsius <- conv_unit(df_temp$temperature, "F", "C")
#figure out how to use month.abb
?month.abb
#convert the numeric month into abbreviations, and order them
df_temp$month <- factor(df_temp$month, levels = 1:12, labels = month.abb)
#library ggplot2 package
library(ggplot2)
#create a ggplot, mapping the variables
ggplot(df_temp, aes(x= celsius, fill = month))+
#add a geometry of histogram, and set the binwidth and boundary color
#Actually, the visualization fits well with 1 degree Celsius as the binwidth
  geom_histogram(colour = "black", binwidth = 1)+
#edit labels of axis and plot
  xlab("Temperature in Celsius")+
  ylab("Count")+
  ggtitle("Temperature by Month")+
#adjust the text size of axis title
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
#check the range of temperature
range(df_temp$celsius)
