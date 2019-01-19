#------------------------------------
# EXAMPLE EXPLORATORY DATA ANALYSIS
#        IN   R
#------------------------------------

#import packages
#install.packages ("readxl")
#install.packages("pastecs")
#install.packages("corrgram")
#load packages
library(readxl)
library(dplyr)
library(pastecs)
library(ggplot2)
library(corrgram)

#Set the working directory
setwd("/Users/OikoEco/R/")
getwd()
#
#-------------------------------
#Import the Data and Formatting
#-------------------------------
StoreData1 <- read_xlsx("StoreData.xlsx")  #excel file
StoreData2 <- read.csv("https://raw.githubusercontent.com/mapguyusa/R-examples/master/StoreData.csv") #CSV File
StoreData3 <- read.table ("https://raw.githubusercontent.com/mapguyusa/R-examples/master/StoreData.txt", sep="\t", header=TRUE)

head(StoreData1)
head(StoreData2)
head(StoreData3)

#Test variable types
sapply (StoreData1, typeof)
sapply (StoreData2, typeof)
sapply (StoreData3, typeof)

is.numeric(StoreData2$Sales) #should be TRUE
is.numeric(StoreData2$SF) #should be TRUE

StoreData2$SQFT <- as.double(StoreData2$SF)

#drop the old SF Variable
StoreData2 <- subset(StoreData2, select = c("Store", "Sales", "SQFT"))
#
#-----------------------
#Descriptive Statistics
#-----------------------
options(scipen=100)
options(digits=2)
stat.desc(StoreData1) #from pastecs package
summary(StoreData1)

#-----------------------
# Histogram
#-----------------------
# Basic histogram
hist <- ggplot(StoreData1, aes(x=Sales)) + geom_histogram(
  binwidth=15000, color="black", fill="white"
)
hist
# Add mean line
hist + geom_vline(aes(xintercept=mean(Sales)),
              color="blue", linetype="dashed", size=2)
#
#----------------------
# Scatter Plot
#----------------------
ggplot(StoreData1, aes(SF, Sales, color = Sales)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .5) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")
#
#----------------------
# Correlation
#----------------------
cor(StoreData1, method="pearson") #correlation coefficients
cor.test(StoreData1$Sales, StoreData1$SF) #correlation test with p-value
#correlogram
library(corrgram)
corrgram(mtcars, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Milage Data (unsorted)")
#
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="Correlogram of Car Mileage Data (PC2/PC1 Order)")
