#######################################
############### ANOVA #################
#######################################

#set working directory 
#for Emily's computer: setwd("C:/Users/Vostro 3550/Desktop/Junior 18-19/Biocomputing/Biocomp_Folder/biocomputing_StatsGroupProject")

#load data 
antibiotics = read.csv(file="antibiotics.csv")

#make a new column for x1 and fill with 0,1 values
x1 = c(0)
antibiotics$x1 = x1
x1.1 = c(1)
antibiotics[5:8,3] = x1.1
x1.2= c(0)
antibiotics[9:12,3] = x1.2
x1.3 = c(1)
antibiotics[13:16,3] =x1.3

#make a new column for x2 and fill with 0,1 values
x2 = c(0)
antibiotics$x2 = x2
x2.2 = c(1)
antibiotics[9:16,4] = x2.2

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


# estimate parameters#where it starts/initial conditions
#b0 20
complexGuess=c(12,12,1)
simpleGuess=c(12,1)

fitComplex=optim(par=complexGuess,fn=complexMod,x=data$treat,y=data$monthsSurvived)
fitSimple=optim(par=simpleGuess,fn=simpleMod,x=data$treat,y=data$monthsSurvived)

# run likelihood ratio test
teststat=2*(fitSimple$value-fitComplex$value)

df=length(fitComplex$par)-length(fitSimple$par)

1-pchisq(teststat,df)

