###########################################################################################
#Script for the R workshop held in conjunction with the OBBN Annual Biomonitoring Meeting
#January 18 2017
#
#Part 1: Introdution to R
#Prepared by: Patrick Schaefer and Chris Jones
###########################################################################################

###########################################################################################
#Prologue 
#
#R is an interprative programming language consisting of objects and functions. Objects are created
#by the user or by function:
a<-NA
a<-"Patrick"
#Here I created a blank object "a", then assigned it to represent the characters "Patrick".
#The "<-" or "=" notation is used to assign something to an object and can create the object in the same process.
b=" is a stats wizard?" 
#Here b is created and assigned in a single line.
#
#Calling the object a or b is done by typing the object name in the console below. It can also be called
#by highlighting the object in the script and pressing ctrl-r, or clicking the cursor on the line containing
#the object and pressing ctrl-r.
a
b
#
#A function is an object that is assigned parameters and will do something with them. Functions accept parameters
#in parenthesis i.e. do.something(). The paste() function will print the assigned values of all objects you give it:
paste(a, b)
#Function outputs can be saved to objects as well and run later to retrieve the values:
c<-paste(a, b)
c
#
#Functions can be called as a parameter to be fed to other functions for more complex interactions:
paste(a, gsub(pattern="stats wizard", replace="nerd", x=b))
#Here, the function gsub() replaced the "stats wizard" text with "nerd" in the object b, within the paste() function.
#Note that b was not permanently altered:
b
#
#There are thousands (maybe millions) of functions that have been created by users and are freely available.
###########################################################################################

###########################################################################################
#Section 1 - Installing and loading packages, Loading Data, Data Types
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

#A factor - type vector contains a set of numeric codes with character-valued levels.
data1$Site
data1$Site[1]
summary(data1$Site)

#Character/string – each element in the vector is a string of one or more characters.
as.character(data1$Site)

###########################################################################################
#Section 2 - Selecting viewing and manipulating data, logical statements
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

#Data summaries
summary(data1)
min(data1$Richness)
max(data1$Richness)
median(data1$Richness)

#Simple xy plot
plot(x=data1$Richness, y=data1$HBI)

plot(x=data1$Richness, y=data1$HBI, 
     main="Richness vs. HBI",
     xlab="Richness",
     ylab="HBI",
     pch=19,
     cex=0.5)
#add a linear trend line
abline(lm(data1$HBI~data1$Richness))

#Histogram
hist(data1$HBI,
     main="HBI Frequency")

#Boxplot
boxplot(data1$HBI~data1$Site,
        main="HBI by Site")

#Using boxplots to identify outliers
outliers<-boxplot(data1$HBI~data1$Site)

outliers
?boxplot
names(outliers)

#Multipanel plotting (easy version)
lattice::xyplot(Richness~MonYr|Site, data=data1, type=c("p","r"))

##########################################################################################
#Section 4 - Loops
#
##########################################################################################

#Common structure of a loop
for (i in 1:10) {
  print(i)
}

#Can run multiple lines of code in a loop
for (i in 1:10) {
  print(i)
  print (i*i)
  print("")
}

#i can take numeric or character form
for (i in c("a","b","c")){
  print(i)
}

output<-data.frame(matrix(nrow=length(unique(data1$Site)), ncol=3))
rownames(output)<-unique(data1$Site)
colnames(output)<-c("Mean","Max","SD")

for (i in unique(data1$Site)){
  output[i,"Mean"]<-mean(data1$HBI[data1$Site==i])
  output[i,"Max"]<-max(data1$HBI[data1$Site==i])
  output[i,"SD"]<-sd(data1$HBI[data1$Site==i])
}

#Write a loop that creates site grouped boxplots for each metric
#i.e. >boxplot(data1$HBI~data1$Site,main="HBI by Site")
#Bonus points for correctly labeling axis and main title
