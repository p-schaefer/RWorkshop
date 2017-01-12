# This script calls-in a dataset of 107 lakes, which are described by 
# a series of hydrologic, geographic, land-use, and land-cover predictors.
# X and Y datasets are prepared, summary stats for these responses and predictors are exported,
# and random forest models are calculated for each of 8 chemical analytes:
# (i.e. ALKT, Ca, Cl, COND, DOC, TKN, pH, TP)
#
# The random forest's predicted values are compared with observed values to assess model fit.
# The script also extracts variable importances for each model, as well as
# partial dependence plots for each predictor in each model. These important
# diagnostics are saved to the working directory (463 files in total)
#
# Coded by FC Jones, January 2017
# f.chris.jones@ontario.ca, 705 766-1724

# create a directory where you want results written to, and adjust this line so it points there
setwd("U:/conferences_workshops_Meetings_Courses/2017/January Biomonitoring Meeting/Jones workshop materials/RandomForest")

# ensure that required packages are loaded
library(pastecs)
library(randomForest)
library(ggplot2)
library(plyr)
library(dplyr)
library(vegan)
library(plotrix)
library(tidyr)

#Call-in data
Lake107<-read.csv("U:/conferences_workshops_Meetings_Courses/2017/January Biomonitoring Meeting/Jones workshop materials/LakeData.csv");View(Lake107)

# Use exported summary stats to check for NAs and invariant variables
SummaryLake107<-as.data.frame(stat.desc(Lake107))
write.csv(SummaryLake107,"107LakeRF_SummaryStats.csv")

# Prepare X and Y datasets: omit unwanted predictors, and filter out Y variables having fewer than 20 unique values
Lake107Chem<-select(Lake107, +UID:elev, +ALKT:TP)
X<-Lake107Chem[,4:58]
Y<-Lake107Chem[,59:66]
#Important for biolgical response variables....make sure there is enough non-zero values to model
#Y<-Y %>% select(which(summarise_each(.,funs = "n_distinct") > 20))

#write prepared X and Y datasets for later reference (good for making raw data appendix or 
# electronic supplement for paper, and for making sure X and Y matrices were built as intended)
write.csv(X, "XChem107.csv")
write.csv(Y, "YChem107.csv")

############################################################
###########################################################
# Create the random forest models, generate all plots, and write them 
# to the working directory (Note: approximately  3 min run time!!)

#Create lists of names, and table templates which will be used by loop...
LBNames<-colnames(Y)        # create sequence of Y variable names (these will be cycled through using loop, creating model and all diagnostics for each one)
MostImpMSE<-data.frame(seq(1:10))   #blank table to which variable importances will be appended later
names(MostImpMSE)<-"Rank"
RFStats<-data.frame(seq(1:4))   # blank table to which stats about model creation and performance will be added later
rownames(RFStats)<-c("MSE", "pseudo r2","r2 (pred vs obs)","mtry")
for (i in seq_along(LBNames)){         #sets up loop that will cycle through Y variables
  #i<-1;j<-1 # set this line as comment when not in development mode
  ModelName<-paste("RF","107Lakes",LBNames[i]) #creates a name for each iteration of the loop, important part of filenames of objects that get saved
  #Run RF
  RF<-randomForest(X,Y[,i], importance=TRUE) # calls RF function from random forest pkg; importance argument ensures that variable importances are calculated
  # Generate out-of-bag random forest predictions for Y
  YRFPred<-predict(RF, type="response")
  # Calclate RF model MSE, rsq and r2 person (as per correl between predicted and response)
  # and write these back to summary table
  r<-cor(Y[,i],YRFPred,method="pearson")
  r2<-round(r^2,digits=2)
  RFStats[1,i+1]<-round(mean(RF$mse),digits=2)
  RFStats[2,i+1]<-round(mean(RF$rsq),digits=2)
  RFStats[3,i+1]<-r2
  RFStats[4,i+1]<-RF$mtry
  colnames(RFStats)[i+1] <- LBNames[i]
  # Create plot of predicted vs observed observations
  lm<-lm(formula=YRFPred~Y[,i])
  n<-length(Y[,i])
  minx<-min(Y[,i])
  maxx<-max(Y[,i])
  rangex<-abs(minx-maxx)
  maxy<-max(YRFPred)
  if (r>0) {         #adjust where plot text is placed so it plots in blank space (depends on whether slope is positive or negative)
    xtext<-minx
  } else {
    xtext<-maxx
  }
  filename5<-paste(ModelName,"predicted vs observed plot.jpg")     # note that filename is concatenation of model name and additional text
  jpeg(filename5)     # opens .jpg-save device which will save plot to be created next
  plot(Y[,i], YRFPred, xlab=paste(LBNames[i],": observed value"), 
       ylab=paste(LBNames[i],": value predicted by random forest"))
  abline(a=0,b=1, lty="dashed")     # adds  1:1 (y=x) line to plot
  abline(a=lm$coefficients[1],b=lm$coefficients[2])  # adds best-fit least squares regression line to plot
  text(x=xtext+(0.1*rangex),y=maxy*0.8, labels=paste("r2 = ",r2),pos=3)  #adds R2 to plot
  text(x=xtext+(0.1*rangex),y=maxy*0.8, labels=paste("n = ",n,"lakes"),pos=1)  #adds n to plot
  dev.off()     # turns off .jpg-save device
  
  # Wrangle the variable-importance data for the most important predictors
  Imp<-as.data.frame(importance(RF)) #extract importances from RF object as a data frame
  names(Imp) <- c("MSE","Purity")    #re-name columns
  Imp$Var<-rownames(Imp)            #add variable names
  Imp<-select(Imp,+Var,+MSE)         # select and re-order columns
  Imp<-arrange(Imp,desc(MSE))        # sort rows in descending order by MSE
  Imp$MSE<-round(Imp$MSE, digits=1)  # round MSE to single decimal place
  Imp$Var.MSE<-paste(Imp$Var,"(",Imp$MSE,")")  # concatenate variable name and MSE 
  TopTen<-Imp$Var.MSE[1:10]          # compile top 10 into a table
  MostImpMSE$Temp<-TopTen            # add as column to blank table created earlier
  colnames(MostImpMSE)[i+1] <- ModelName
  #Variable Importance Plot
  ggplot(Imp, aes(reorder(Var, MSE),MSE)) + geom_point() + geom_hline(yintercept = 0)+ coord_flip() +
    xlab("Variable")+ ylab("Mean Increase in Model Accuracy (MSE)")+
    theme(axis.text=element_text(size=9),axis.title=element_text(size=14,face="bold"))+
    theme_bw()+theme(panel.grid.major=element_line(size=1))     # ggplot is very versatile. Learn to use it
  filename<-paste(ModelName, "VarImp Plot.jpg")
  ggsave(filename, width=4.5, height=12, units="in")  #quick and easy way to save objects created by ggplot
  # Partial Dependence Plots
  for(j in seq_along(colnames(X))){  # this is another loop within the loop we already created! Cycles through the predictors so a partial dependence plot is created for each one
    set.seed(121)   # seed for permutation, ensures that output is reproducible
    filename1<-paste(ModelName,"Partial Dep Plot", colnames(X[j]), ".jpg")
    jpeg(filename1)
    partialPlot(RF, X, colnames(X)[j], xlab=colnames(X)[j], ylab=paste("Mean Prediction of", LBNames[i]), main=NULL)
    dev.off()
  }
  
}

write.csv(MostImpMSE,"107LakesChemistry_VariableImportances_AllYSummary.csv")  # save table of variable importances
write.csv(RFStats,"107LakesChemistry_RandomForestModelStats.csv")              # save table of model-performance stats

#Create plot to compare pseudo-R2 for all RF models
RFMSE<-select(RFStats[2,],-seq.1.4.)%>%gather(., Var, Value)%>%arrange(.,desc(Value)) #wrangle the pseudo-r2 data
ggplot(RFMSE, aes(reorder(Var, Value),Value)) + geom_point() +  coord_flip() +
  xlab("Variable")+ ylab("Random Forest Mean Pseudo R-squared")+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=14,face="bold"))+
  theme_bw()+theme(panel.grid.major=element_line(size=1))
filename6<-"107 Lakes Chemistry RF model R-squares.jpg"
ggsave(filename6, width=4.5, height=11, units="in")

