###########################################################################################
#Script for the R workshop held in conjunction with the OBBN Annual Biomonitoring Meeting
#January x 2017
#
#Part 2: Best statistical practices for benthic biomonitoring
#Prepared by: Patrick Schaefer and Chris Jones
###########################################################################################

###########################################################################################
#The BenthicAnalysis package is a work in progress
#
#It will hopefully be a single tool that can handle biotic, geospatial and environmental data
#to report on stream quality using Benthic Macroinvertebrates
###########################################################################################
install.packages("devtools")
library(devtools)
install_github("p-schaefer/BenthicAnalysis", dependencies=T, force=T)
library(BenthicAnalysis)

runUI()

#Extract summary statistics
ben<-benth.met(RCAtaxadata,tax.fields = 2, site.fields = 2)
ben.mets<-ben$Summary.Metrics
