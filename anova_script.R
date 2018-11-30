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
#then fill in the 0,1 values for each treatment 
x1.1 = c(1) #create a vector of 1s for the 1 values
antibiotics[5:8,3] = x1.1 #antibiotic treatment 1 is all 1 values, leaving control as 0s
antibiotics[9:12,3] = x1 #antibiotic treatment 2 is all 0s (step not necessary because column is already 0s, but for conceptual purposes)
antibiotics[13:16,3] =x1.1 #antibiotic treatment 3 is all 1 values

#make a new column for x2 and fill with 0,1 values
antibiotics$x2 = x1 #sets the entire column as 0s 
antibiotics[9:16,4] = x1.1 #antibiotic treatments 2 and 3 will be set as 1s, leaving control and antibiotic treatment 1 as 0s 


#create likelihood functions

#create the first, most complicated model 
ThirdMod<-function(p,x1,x2,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  sigma=exp(p[4])
  
  pred=B0+B1*x1+B2*x2
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}

#create the second, less complicated model 
SecondMod<-function(p,x1,x2,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  
  pred=B0+B1*x1
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}

#create the final, simplest model
FirstMod<-function(p,x1,x2,y){
  B0=p[1]
  sigma=exp(p[2])
  
  pred=B0
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}


# estimate parameters for each model- this is where it starts/the initial conditions
# these estimations are based on the averages shown from the plot generated above 
thirdGuess=c(20,-15,-2) #estimations for most complicated ThirdMod
secondGuess=c(20,-15) #estimations for less complicated SecondMod 
firstGuess = c(20) #estimations for simplest FirstMod

#fit for each model
fitthird=optim(par=thirdGuess,fn=ThirdMod,x=antibiotics$trt,y=antibiotics$growth) 
fitsecond=optim(par=secondGuess,fn=SecondMod,x=antibiotics$trt,y=antibiotics$growth)
fitfirst=optim(par=firstGuess,fn=FirstMod,x=antibiotics$trt,y=antibiotics$growth)

# run likelihood ratio test 
teststat=2*(fitSimple$value-fitComplex$value)

df=length(fitComplex$par)-length(fitSimple$par)

1-pchisq(teststat,df)

