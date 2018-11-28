#######################################
############### ANOVA #################
#######################################

#set working directory 
#for Emily's computer: setwd("C:/Users/Vostro 3550/Desktop/Junior 18-19/Biocomputing/Biocomp_Folder/biocomputing_StatsGroupProject")

#load data 
antibiotics = read.csv(file="antibiotics.csv")

#plot the results using ggplot
library(ggplot2)
plot(antibiotics)

#create likelihood functions
ThirdMod<-function(p,x1,x2,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  sigma=exp(p[4])
  
  pred=B0+B1*x1+B2*x2
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}

secondMod<-function(p,x1,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  
  pred=B0+B1*x1
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}

firstMod<-function(p,x,y){
  B0=p[1]
  sigma=exp(p[2])
  
  pred=B0
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}




