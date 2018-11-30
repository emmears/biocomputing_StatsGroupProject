#######################################
############### ANOVA #################
#######################################

#set working directory 
#for Emily's computer: setwd("C:/Users/Vostro 3550/Desktop/Junior 18-19/Biocomputing/Biocomp_Folder/biocomputing_StatsGroupProject")

#load data 
antibiotics = read.csv(file="antibiotics.csv")

#plot the results using ggplot
library(ggplot2)
plotant = plot(antibiotics)
png(paste("C:/Users/Vostro 3550/Desktop/Junior 18-19/Biocomputing/Biocomp_Folder/biocomputing_StatsGroupProject/Antibiotics Plot.png", sep = " "),   height = 768, width=1024)
plotant
dev.off()

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

secondMod<-function(p,x1,x2,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  
  pred=B0+B1*x1
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}

firstMod<-function(p,x1,x2,y){
  B0=p[1]
  sigma=exp(p[2])
  
  pred=B0
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}


# estimate parameters- this is where it starts/initial conditions
#b0 20
thirdGuess=c(20,-15,-2)
secondGuess=c(20,-15)
firstGuess = c(20)

fitthird=optim(par=thirdGuess,fn=thirdMod,x=antibiotics$trt,y=antibiotics$growth)
fitsecond=optim(par=secondGuess,fn=secondMod,x=antibiotics$trt,y=antibiotics$growth)
fitfirst=optim(par=firstGuess,fn=firstMod,x=antibiotics$trt,y=antibiotics$growth)

# run likelihood ratio test
teststat=2*(fitSimple$value-fitComplex$value)

df=length(fitComplex$par)-length(fitSimple$par)

1-pchisq(teststat,df)
