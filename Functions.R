###################################################################
#
#Functions for use in the OBBN R workshop - January 18 2017
#
#Created by: Patrick Schaefer (pschaefer@creditvalleyca.ca)
#
###################################################################

"Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

1) Identification of the creator(s) of the Licensed
Material and any others designated to receive
attribution, in any reasonable manner requested by
the Licensor (including by pseudonym if designated);
  
  The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."

####################################################################


ci_fun2<-function(model,model.variable,data=NULL,col.variable=NULL,data.variable,plot=T,stat="median",level=0.95,oddsRatio=F,nsim=1000,sig.only=FALSE,...){
  require(plotrix)
  require(merTools)
  
  sim<-REsim(model,n.sim=nsim,...)
  
  output<-sim[sim$term==model.variable,]
  output$upper<-output[,stat]+output[,"sd"]*qnorm(1-((1-level)/2))
  output$lower<-output[,stat]-output[,"sd"]*qnorm(1-((1-level)/2))
  output$sig<- output[, "lower"] > 0 | output[, "upper"] < 0
  hlineInt<-0
  if (oddsRatio == TRUE) {
    output[, "ymax"] <- exp(output[, "upper"])
    output[, stat] <- exp(output[, stat])
    output[, "ymin"] <- exp(output[, "lower"])
    hlineInt <- 1
  }
  
  if (plot==T) {
    if (sig.only==T){
      
      plot.output<-output[order(eval(parse(text=paste0('output$',stat)))),]
      plot.output<-plot.output[plot.output$sig==T,]
      sitenum<-nrow(plot.output)
      if (!is.null(data) & !is.null(col.variable)){
        colours<-data[sapply(as.character(plot.output$groupID),function(x) match(x,as.character(eval(parse(text=paste0('data$',data.variable)))))),col.variable]
      }
      
      plotCI(x=1:sitenum,y=plot.output[, stat],
             li=as.numeric(plot.output$lower),
             ui=as.numeric(plot.output$upper),
             lwd=1,xlab="",ylab="site-specific slope (95% CI)",xaxt="n",las=1,main=paste0(colnames(attr(model,"frame"))[1]),
             col=if (!is.null(data)& !is.null(col.variable)) {col=colours} else {"black"})
      axis(1,at=1:sitenum,labels=plot.output$groupID,cex.axis=0.6,las=2)
      abline(h=hlineInt,lty=2,lwd=1)
      if (!is.null(data) & !is.null(col.variable)){
        legend("topleft",c("Lower","Middle","Upper"),pch=c(21),pt.bg=c(1:3),bty="n",cex=1)
      }

    }
    if (sig.only==F) {
      plot.output<-output[order(eval(parse(text=paste0('output$',stat)))),]
      sitenum<-nrow(plot.output)
      if (!is.null(data) & !is.null(col.variable)){
        colours<-data[sapply(as.character(plot.output$groupID),function(x) match(x,as.character(eval(parse(text=paste0('data$',data.variable)))))),col.variable]
      }
      
      plotCI(x=1:sitenum,y=plot.output[, stat],
             li=as.numeric(plot.output$lower),
             ui=as.numeric(plot.output$upper),
             lwd=1,xlab="",ylab="site-specific slope (95% CI)",xaxt="n",las=1,main=paste0(colnames(attr(model,"frame"))[1]),
             col=if (!is.null(data) & !is.null(col.variable)) {col=colours} else {"black"})
      axis(1,at=1:sitenum,labels=plot.output$groupID,cex.axis=0.6,las=2)
      abline(h=hlineInt,lty=2,lwd=1)
      if (!is.null(data) & !is.null(col.variable)){
        legend("topleft",c("Lower","Middle","Upper"),pch=c(21),pt.bg=c(1:3),bty="n",cex=1)
      }
    }
  }
  return(output)
}

xyplot_fun2<-function(model,model.variable,data,data.variable="Site",...) {
  require(lattice)
  
  data$Site<-as.factor(as.character(eval(parse(text=paste0('data$',data.variable)))))
  
  temp.data<-data
  
  ci.mod<-ci_fun2(model=model,model.variable=model.variable,data.variable=data.variable,data=data,plot=F,...)
  
  temp.data$Site.sig<-temp.data$Site
  if (any(ci.mod$upper>0&ci.mod$lower>0)){
    levels(temp.data$Site.sig)[which(levels(temp.data$Site.sig)%in%ci.mod$groupID[(ci.mod$upper>0&ci.mod$lower>0)])]<-unique(paste0(unlist(levels(temp.data$Site.sig)[which(levels(temp.data$Site.sig)%in%ci.mod$groupID[(ci.mod$upper>0&ci.mod$lower>0)])]), "*+*"))
  }
  if (any(ci.mod$upper<0&ci.mod$lower<0)){
    levels(temp.data$Site.sig)[which(levels(temp.data$Site.sig)%in%ci.mod$groupID[(ci.mod$upper<0&ci.mod$lower<0)])]<-unique(paste0(unlist(levels(temp.data$Site.sig)[which(levels(temp.data$Site.sig)%in%ci.mod$groupID[(ci.mod$upper<0&ci.mod$lower<0)])]), "*-*"))
  }
  xyplot(formula(paste0(colnames(model@frame)[1],"~ Year | Site.sig")),
         data=temp.data,auto.key=T,type=c("p","r"))
}
