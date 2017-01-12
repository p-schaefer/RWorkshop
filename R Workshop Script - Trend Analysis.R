###########################################################################################
#Script for the R workshop held in conjunction with the OBBN Annual Biomonitoring Meeting
#January 18 2017
#
#Part 2: Best statistical practices for benthic biomonitoring
#Prepared by: Patrick Schaefer (pschaefer@creditvalleyca.ca)
###########################################################################################

###########################################################################################
#Scenario 2: Temporal Trend Analysis - A framework for multiscale trend analysis
#
#Questions: 
#1) Are metrics at individual sites increasing or decreasing significantly?
#2) Is there evidence for declines on a watershed/subwatershed scale?
#3) Are some watersheds/subwatersheds significantly better or worse than others?
#
#Study Design:
#Long term environmental monitoring, repeated sampling at sites, stratified random sampling,
#minimum data requirments depend on questions being asked
#
#Example dataset: trenddata.csv
###########################################################################################

###########################################################################################
#1) Are metrics at individual sites increasing or decreasing significantly?
###########################################################################################
require(lme4)
require(MuMIn)
require(plotrix)
require(merTools)
require(lattice)
data("trenddata")
set.seed(101)

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
##########################################

#Will only examine HBI (Gaussian) and Intolerants Richness (Poisson)
hist(trenddata$HBI)
hist(trenddata$Intolerants.Richness)


##########################################
#Step 2 - Determine fixed and random predictor variables
#
#Because we are only interested in the effect of time, including too many additional predictors will
#limit the interprative power of time.
#
#If building multiple models, keep pool of predictor variables consistant between models, or it will be difficult to
#compare results
##########################################

#Site specific trend models
site.model1<-lmer(HBI~1+(MonYr|Site)+(1|Year.factor),
                  data=trenddata,
                  na.action=na.fail,
                  REML=F)

site.model2<-glmer(EPT.Richness~1+(MonYr|Site)+(1|Year.factor),
                   data=trenddata,
                   family=poisson,
                   na.action=na.fail)

##########################################
#Step 3 - Model Validation and Checking model assumptions
#
#Using models standardized residuals to check for normality, homoscedasticity, outliers and influential values
##########################################

#Check Model Assumptions
hist(resid(site.model1,type="pearson")) # The residual frequency distribution should vary around zero, showing no substantial skewness pattern
scatter.smooth(x=fitted(site.model1), y=resid(site.model1,type="pearson")) #The plot of standardised residuals (model.res) against fitted response values (fitted(model1)) must not show any change in the residual's variability along the fitted values
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
scatter.smooth(x=trenddata$EPT.Richness, y=fitted(site.model2))
xyplot(trenddata$EPT.Richness~trenddata$MonYr|trenddata$Site)
xyplot(fitted(site.model2)~trenddata$MonYr|trenddata$Site)

#########################################
#Step 4 - Interpret Results
#
#Examine 95% confidence-intervals for random effect slope parameters. If they dont overlap 0 there is a significant
#trend at that site
#########################################

ci1<-ci_fun2(model=site.model1,
             model.variable="MonYr",
             data.variable="Site",
             col.variable="Zone",
             plot=T,
             stat="median",
             level=0.95,
             data=trenddata)

ci2<-ci_fun2(model=site.model2,
             model.variable="MonYr",
             data.variable="Site",
             col.variable="Zone",
             plot=T,
             stat="median",
             level=0.85,
             data=trenddata)

xyplot_fun2(model=site.model1,
            model.variable="MonYr",
            data.variable="Site",
            data=trenddata)

xyplot_fun2(model=site.model2,
            model.variable="MonYr",
            data.variable="Site",
            level=0.85,
            data=trenddata)

###########################################################################################
#2) Is there evidence for declines on a watershed/subwatershed scale?
#3) Are some watersheds/subwatersheds significantly better or worse than others?
###########################################################################################
require(lme4)
require(MuMIn)
require(plotrix)
require(merTools)
require(lattice)
data("trenddata")
set.seed(101)

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
#Here we are interested in determining if there are significant differences in our metrics in lower (L), middle (M) and
#upper (U) portions of the watershed, and if there is evidence for significant trends in these regions.
#
#Because we are interested in the effect of time and space, these two variables are now added as fixed effects. 
#
##########################################

#Watershed/subwatershed model
watershed.model1<-lmer(HBI~0+MonYr*Zone+(MonYr|Site)+(1|Year.factor),
                       data=trenddata,
                       na.action=na.fail,
                       REML=F)

watershed.model2<-glmer(EPT.Richness~0+MonYr*Zone+(MonYr|Site)+(1|Year.factor)+(1|obs),
                        data=trenddata,
                        na.action=na.fail,
                        family=poisson)

#Here we run into one of the most common warning messages using glmer. Essentially, this warning tells us 
#the heiristic algorithms that estimate model parameters may not have landed on the best parameter estimate.
#see:
?convergence
#and a checklist for troubleshooting convergence errors can be found here:
#https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
#
#In this case we can fix the warning by changing the optimizer:
watershed.model2<-glmer(EPT.Richness~0+MonYr*Zone+(MonYr|Site)+(1|Year.factor)+(1|obs),
                        data=trenddata,
                        na.action=na.fail,
                        family=poisson,
                        glmerControl(optimizer = "bobyqa"))

##########################################
#Step 3 - Model Validation and Checking model assumptions
#
#Using models standardized residuals to check for normality, homoscedasticity, outliers and influential values
##########################################

#Check Model Assumptions
hist(resid(watershed.model1,type="pearson"))
scatter.smooth(x=fitted(watershed.model1), y=resid(watershed.model1,type="pearson"))
qqnorm(resid(watershed.model1))
qqline(resid(watershed.model1))

hist(resid(watershed.model2,type="pearson"))
scatter.smooth(x=fitted(watershed.model2), y=resid(watershed.model2,type="pearson"))
qqnorm(resid(watershed.model2))
qqline(resid(watershed.model2))

#Check Model Fit
summary(watershed.model1)
r.squaredGLMM(watershed.model1)
scatter.smooth(x=trenddata$HBI, y=fitted(watershed.model1))
xyplot(trenddata$HBI~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)
xyplot(fitted(watershed.model1)~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)

summary(watershed.model2)
r.squaredGLMM(watershed.model2)
scatter.smooth(x=trenddata$EPT.Richness, y=fitted(watershed.model2))
xyplot(trenddata$EPT.Richness~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)
xyplot(fitted(watershed.model2)~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)

##########################################
#Step 4 - Multi-model inference (part 1)
#
#Using more classical hypothesis testing, we could test the significance of the time and space parameters in the previous model as well as
#their interactions to gain insights into their importance. For many reasons (that are beyond the scope of this workshop),
#this is not the approach that should be taken. 

#Simply, parameter significance depends on what other parameters are in the model. This complicates matters when there isn't overwhelming
#support for a single model. Multiple models may be nearly equally good at describing variation in the dataset. A more objective approach is
#to build multiple models that provide a concensus of variable importance. 
#
#The dredge() function takes the full (i.e. most complex, including all terms of interest and interactions) model and automatically
#builds all combinations of simpler models from the provided terms. For each model AIC values are computed where measure the model fit
#in relation to the numer of parameters used to achieve the fit. 
##########################################

mm1<-dredge(watershed.model1,rank="AIC")
mm1

mm2<-dredge(watershed.model2,rank="AIC")
mm2
########################################

##########################################
#Step 4 - Multi-model inference (part 2)
#
#In both models, there is stronger support for the top fitting model than the rest, but the difference is not so great that subsequent models 
#should not be evaluated. the subset argument lets you test different thresholds of ΔAIC to test:
# i.e. ΔAIC ≤ 2, ΔAIC ≤ 4, ΔAIC ≤ 6, cumulative AICw ≤ 0.95
#
##########################################

top.models1<-get.models(mm1,subset=cumsum(mm1$weight)<=0.95)
top.models2<-get.models(mm2,subset=mm2$delta<4)

##########################################
#Step 5 - Multi-model inference (part 3)
#
#Once we have selected our top models, we can avergae parameter estimates from top models to better understand their importance.
#
#There are two methods to average model coefficients and their errors:
#The natural method provides a weighted mean for each variable coefficient and its errors, based on AICw values, but only,
#if this variable occurs in a model. Variables not occurring in a model are neglected. In contrast, the zero method
#calculates a weighted mean for coefficients and errors, while substituting variable coefficients absent in a model by zero.
#
#Note that the argument revised.var=TRUE provides a corrected estimation of the standard errors. The function
#summary (mm1.av) provides the model outputs for the natural (called subset/conditional) and zero methods (called full), including averaged
#model parameters (SES), errors and significance
##########################################

mm1.av<-model.avg(get.models(mm1,subset=cumsum(mm1$weight)<=0.95), revised.var=T)
mm2.av<-model.avg(get.models(mm2,subset=mm2$delta<8), revised.var=T)

summary(mm1.av)
summary(mm2.av)

xyplot(trenddata$HBI~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)
xyplot(trenddata$EPT.Richness~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)
xyplot(fitted(watershed.model2)~trenddata$MonYr|trenddata$Zone,groups=trenddata$Site)

##########################################
#Step 6 - Interpreting parameter estimates
#
#For lmer models, parameter interpretation is fairly easy if you fit each level of the model with its own intercept term 
#(i.e. response~1+predictors, vs. response~0+predictors).
summary(mm1.av)
confint(mm1.av,full=F)
#There is strong evidence for differences between zones (L=6.7,M=5.4,U=5.7) - parameter estimates can be treated as model derived averages
#
#Because monyr*zone interactions were significant in at least one of the selected models, there is no global slope
#parameter estimate, only zone specific slopes. The parameter estimate "MonYr" is the slope in the reference 
#treatment (i.e. the first in the list or L here). Here HBI decreases by 0.02 per year in the Lower zone. Parameter estimates in
#the other 2 zones are also based on the slope in the reference zone (i.e. the slope in the upper zone = "MonYr" + "MonYr:ZoneM").
#Using the more conservative full average, U zone slope is = -0.024848+-0.003120 =  -0.027968, and M zone slope is = -0.024848 + 0.003259 = -0.021589
#U and M zones slopes are not significantly different from the refernce zone (L) slope, and the relative importance of the MonYr*Zone interaction
#is quite low. This indicates zonal differences in slope are likely not signifcant.
#
#The ecological interpretation is that there are significant differences in HBI values between zones. There is also evidence for a slight
#decline in HBI values across the watershed (~0.02 per year). This trend was not significantly different in different parts of the watershed.
#
#NOT: If you did not fit a intercept term to each level of the model (i.e. response~1+predictors), then grouping parameters need to be treated like 
#slope was above (i.e. each level of the treatment is relative to the reference group)
#
###############
#
#For glmer models, interpretation is a little more dificult.
summary(mm2.av)
confint(mm1.av,full=F)
#Group/intercept estimates need to be back transformed based on the family used in the model. For default: Poisson use exp(x), for binomial use  1/(1+1/exp(x)).
coef(mm2.av,full=F)[c("ZoneL","ZoneM","ZoneU")]
exp(coef(mm2.av,full=F)[c("ZoneL","ZoneM","ZoneU")])
#The average number of EPT taxa is 3.6 in the L zone, and ~6.4 for M and U. Significant differences detected between zones.
#
#Slope estimates are interpreted as % changes when raw and as multiplicative factors when exponentiated.
coef(mm2.av,full=F)["MonYr"]
coef(mm2.av,full=F)["MonYr"]+coef(mm2.av,full=F)["MonYr:ZoneU"]
exp(coef(mm2.av,full=F)["MonYr"])
exp(coef(mm2.av,full=F)["MonYr"]+coef(mm2.av,full=F)["MonYr:ZoneU"])
#In the L zone, EPT richness is decreasing by ~1% every year or multiplied by ~0.99. Alternatively, in the upper zone,
#richness is increasing by ~2% or multiplied by 1.02. In this case, the model with MonYr and the MonYr*Zone interaction performed
#considerably better than the model without, resulting in higher relative importance of the temporal trend, and different trends between zones.
#
#Ecological Interpretation: There are significant differences in intolerant richness between zones. There is evidence for different trends in
#different parts of the watershed. Specifically, it appears the U zone is increasing in intolerants richness relative to M and L zones. 
#
##########################################
