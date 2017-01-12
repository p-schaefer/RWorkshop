###########################################################################################
#Script for the R workshop held in conjunction with the OBBN Annual Biomonitoring Meeting
#January 18 2017
#
#Part 2: Best statistical practices for benthic biomonitoring
#Prepared by: Patrick Schaefer (pschaefer@creditvalleyca.ca)
###########################################################################################

###########################################################################################
#Scenario 1: RCA Bioassessment - The BenthicAnalysis package (a work in progress)
#
#It will hopefully be a single tool that can handle biotic, geospatial and environmental data
#to report on stream quality
###########################################################################################
install.packages("devtools")
library(devtools)
install_github("p-schaefer/BenthicAnalysis", dependencies=T, force=T)
library(BenthicAnalysis)

runUI()

data("RCAHabitatData")
data("RCATaxaData")
data("RCASiteClass")
data("RCAManualSiteMatch")