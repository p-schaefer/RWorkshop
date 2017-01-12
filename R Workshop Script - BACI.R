# Script for analyzing data from a Before-After-Control-Impact study (example: new dam built on a river)
# 
# The BACI design allows one to look for non-parallel responses of upstream (control) and downstream (impacted) sites over time. 
# This is important because temporal change may be confounded by dam effects, and 
# differences between upstream and downstream reaches may be unrelated to the dam.
# The design is most appropriate when large and long lasting effects (change in mean value) are of concern.
# Assesment is done by way of 2-factor ANOVA (factor 1 is Time: before/after, factor 2 is impact: control/impact)
# The Time X Impact interaction is of interest.
#
# Script does the followiong things:
# 1. Creates simulated dataset arising from the (BACI) study of a new dam. 
# In this hypothetical study, 5 locations in each of two river reaches (i.e., one upstream of proposed dam, 
# and one downstream) have been sampled at two points in time 
# (i.e., before construction and after construction).
# The response variable of interest is abundance of benthic invertebrates collected using a surber sampler
#
# 2. Demonstrates standard ANOVA-based analysis of BACI dataset, using simulated dataset as input
# - summarizes and creates plot by which spatial and temporal patterns of response variable's variation can be visualized
# - creates standard diagnostic plots that are used to evaluate dataset's agreement with ANOVA assumptions 
#   (i.e. equal variances in the differnt factor combinations, and randomly distributed residuals)
# - specifies and solves ANOVA model appropriate to 2-factor design with interaction
# 
# Script created by C Jones, January 2017
# f.chris.jones@ontario.ca
# 705 766-1724

# Set working directory (change this so it points to directory where you downloaded scripts and input files to)
setwd("U:/conferences_workshops_Meetings_Courses/2017/January Biomonitoring Meeting/Jones workshop materials")

# Load required libraries
library(dplyr)
library(ggplot2)
library(plyr)

#simulate dataset
Data<-data.frame(Sample=1:20, Time=c("B","A"))
Data<-Data%>%arrange(desc(Time))
imp=c("C","C","C","C","C","I", "I","I","I","I")
Data$Impact<-imp
Data$Impact<-as.factor(Data$Impact)
BC<-rnorm(5, mean = 15000, sd = 2000) #simulate invertebrate abundances as random normal variables
BI<-rnorm(5, mean = 12000, sd = 2000)
AC<-rnorm(5, mean = 14000, sd = 2000)
AI<-rnorm(5, mean = 6000, sd = 2000)
abund<-c(BC, BI, AC, AI)   # concatenate simulated abundances into a single vector
Data$Abundance<-abund      # write vector of abundances back to Data file

#visualize the data by creating plot that illustrates the distribution of abundance values
# observed in each treatment group, and save the plot to working directory
filename1<-"InteractionPlot.jpg"   # create name for saved plot
jpeg(filename1)                    # turn on .jpg-save device
interaction.plot(Data$Time, Data$Impact, Data$Abundance, trace.label=NULL) #calls standard plotting function
dev.off()     #turns of .jpg-save device
ggplot(Data, aes(Time, Abundance, color = Impact)) + geom_point(size=5)+scale_fill_grey()+theme_bw() #use ggplot. Very versatile
filename2<-"2WayANOVADataSummary.jpg"
ggsave(filename2, width=5, height=5, units="in")   #quick method for saving ggplot object

# Calculate ANOVA
ANOVA<-aov(Data$Abundance ~ Data$Time*Data$Impact, data=Data) #calls standard function. AOV creates a large list of results
# use indexing to show differnt parts of list
ANOVA[1]    # same as ANOVA$coefficients (coefficients of the least squares fit of the response(s) on the model matrix)
ANOVA[2]    # same as ANOVA$residuals
ANOVA[3]    # same as ANOVA$effects (orthogonal, single-degree-of-freedom effects)
ANOVA[4]    # same as ANOVA$rank (model's number of estimable effects)
ANOVA[5]    # same as ANVOA$fitted.values ()
ANOVA[6]    # same as ANOVA$assign (assigns coefficients (and effects) to terms in the model)
ANOVA[7]    # same as ANOVA$coefficients...another list (coefficients of the least squares fit of the response(s) on the model matrix)
ANOVA[8]    # same as ANOVA$df.residual (residuals' degrees of freedom)
ANOVA[9]    # same as ANOVA$contrasts (contrasts specified in the model)
ANOVA[10]   # same as ANOVA$xlevels (list of levels associated with factors included in the model)
ANOVA[11]   # same as ANOVA$call (reminds one of the arguments used to specify the model using aov function)
ANOVA[12]   # same as ANOVA$xlevels 
ANOVA[13]   # same as ANOVA$terms (another specification of the model, used internally in R)

#write ANOVA table to working directory
ANOVAtable<-summary(ANOVA)
capture.output(ANOVAtable,file="ANOVAtable.csv") #quick way to export table (which is a list object)







# Plot standard diagnostics to check for heteroscedasticity, normality, and influential observerations
filename3<-"ANOVADiagnostics.jpg"
jpeg(filename3)
par(mfrow=c(2,2))
plot(ANOVA)
dev.off()