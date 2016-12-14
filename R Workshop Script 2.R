###########################################################################################
#Script for the R workshop held in conjunction with the OBBN Annual Biomonitoring Meeting
#January x 2017
#
#Part 2: Best statistical practices for benthic biomonitoring
#Prepared by: Patrick Schaefer and Chris Jones
###########################################################################################

###########################################################################################
#Scenario 1: RCA Bioassessment - The BenthicAnalysis package (a work in progress)
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



###########################################################################################
#Scenario 2: Temporal Trend Analysis - A framework for multiscale trend analysis
#
#Questions: 
#1) Are metrics at individual sites increasing or decreasing significantly?
#2) Is there evidence for declines on a watershed/subwatershed scale?
#3) Are some watersheds/subwatersheds significantly better or worse than others?
#
#Study Design:
#Long term environmental monitoring, repeated sampling at consistent sites, stratified random sampling,
#minimum data requirments depend on questions being asked
#
#Example dataset: trenddata.csv
###########################################################################################
require(lme4)
require(MuMIn)
require(plotrix)
require(merTools)

head(trenddata)
str(trenddata)
trenddata$Year.factor<-as.factor(trenddata$Year.factor)
trenddata$Site<-as.factor(trenddata$Site)

#########################################
#Step 1 - Determine the response distribution
#
#Usually best to use theoretical understanding of variables rather than actual distributions:
# - Binomial - presence/absence or percentages
# - Poisson - counts, richness (mean<8)
# - Gausian (or normal) - HBI, ratios (sometimes), richness (mean >10)
#Will only examine HBI (Gaussian) and Percent.ICHAEBO (Binomial)
##########################################

##########################################
#Step 2 - Determine fixed and random predictor variables
#
#Because we are only interested in the effect of time, including too many additional predictors will
#limit the interprative power of time.
#
#If building multiple models, keep pool of predictor variables consistant between models, or it will be difficult to
#compare models.
##########################################

#Site specific trend models
site.model1<-lmer(HBI~1+(MonYr|Site),
                  data=trenddata,
                  na.action=na.fail,
                  REML=F)

site.model2<-glmer(Intolerants.Richness~1+(MonYr|Site),
                  data=trenddata,
                  family=poisson,
                  na.action=na.fail)

##########################################
#Step 3 - Model Validation and Checking model assumptions
#
#Using models standardized residuals to check for normality, homoscedasticity, outliers and influential values
##########################################

#Check Model Assumptions
hist(resid(site.model1,type="pearson"))
scatter.smooth(x=fitted(site.model1), y=resid(site.model1,type="pearson"))
qqnorm(resid(site.model1))
qqline(resid(site.model1))

hist(resid(site.model2,type="pearson"))
scatter.smooth(x=fitted(site.model2), y=resid(site.model2,type="pearson"))
qqnorm(resid(site.model2))
qqline(resid(site.model2))


#Check Model Fit
summary(site.model1)
r.squaredGLMM(site.model1)
scatter.smooth(x=trenddata$HBI, y=fitted(site.model1))
xyplot(trenddata$HBI~trenddata$MonYr|trenddata$Site)
xyplot(fitted(site.model1)~trenddata$MonYr|trenddata$Site)

summary(site.model2)
r.squaredGLMM(site.model2)
scatter.smooth(x=trenddata$Intolerants.Richness, y=fitted(site.model2))
xyplot(trenddata$Intolerants.Richness~trenddata$MonYr|trenddata$Site)
xyplot(fitted(site.model2)~trenddata$MonYr|trenddata$Site)

#########################################
#Step 4 - Interpret Results
#
#Examine 95% confidence-intervals for random effect slope parameters. If they dont overlap 0 there is a significant
#trend at that site
#########################################

ci1<-ci_fun2(model=site.model1,model.variable="MonYr",data=trenddata,plot=T)
ci2<-ci_fun2(model=site.model2,model.variable="MonYr",data=trenddata,plot=T)

xyplot_fun2(model=site.model1,model.variable="MonYr",data=trenddata)
xyplot_fun2(model=site.model2,model.variable="MonYr",data=trenddata)


#Watershed/subwatershed model
watershed.model1<-lmer(HBI~0+MonYr*Zone+(MonYr|Site),
                       data=trenddata,
                       na.action=na.fail,
                       REML=F)




