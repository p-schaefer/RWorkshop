###########################################################################################
#Script for the R workshop held in conjunction with the OBBN Annual Biomonitoring Meeting
#January 18 2017
#
#Part 1: Intorudtion to R
#Prepared by: Patrick Schaefer and Chris Jones
###########################################################################################

###########################################################################################
#Section 1 - Installing and loading packages, Loading and Manipulating Data, Data Types
#
###########################################################################################

#Download packages
install.packages(c("vegan","lme4","MuMIn","devtools"))

#Load packages into memory
library(vegan)
library(lme4)
library(MuMIn)
library(devtools)

#Loading data from a .csv file
data1<-read.csv(choose.files())
data1<-read.csv("file path")
data1<-read.delim("clipboard")

#View the data
View(data1)

#Look at the structure of the data
str(data1)

#Data frame vs. Vector
data1$Site

###########################################################################################
#Section 2 - Selecting viewing and manipulating data, data types
#
###########################################################################################

#Selecting a column
data1[,1]
data1[,c(1:3,5)]
data1[,"Richness"]
data1[,c("Richness","HBI")]
data1$Richness

#Selecting data by rows
data1[1:4,]

#Using logical statements
data1$Site == "Site A" # "==" a logical statement - returns T/F vector
data1$Site != "Site A" # "!=" a logical statement - returns T/F vector

data1$MonYr %in% c(2,3) # "%in%" a logical statement - returns T/F vector

#Selecting data by logical vectors
data1[data1$Site =="Site A",]
data1[data1$MonYr %in% c(2,3),]
data1[data1$Site =="Site A" | data1$Site =="Site B",]
data1[data1$Site =="Site A" & data1$MonYr %in% c(2,3),]

#Add a new column to the dataset where year is treated as a factor
data1$MonYr.factor<-as.factor(data1$MonYr)

#Add a new column to the dataset where year is treated as a character vector
data1$MonYr.char<-as.character(data1$MonYr)

str(data1)
View(data1)

#cross-tab counts
table(data1$Site,data1$MonYr)

###########################################################################################
#Section 3 - Data summaries and plots
#
###########################################################################################

summary(data1)
min(data1$Richness)
max(data1$Richness)
median(data1$Richness)

plot(x=data1$Richness, y=data1$HBI)

plot(x=data1$Richness, y=data1$HBI, 
     main="Richness vs. HBI",
     xlab="Richness",
     ylab="HBI",
     pch=19,
     cex=0.5)
abline(lm(data1$HBI~data1$Richness))

hist(data1$HBI)

boxplot(data1$HBI~data1$Site,
        main="HBI by Site")

outliers<-boxplot(data1$HBI~data1$Site)

names(outliers)
outliers
