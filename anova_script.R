#######################################
############### ANOVA #################
#######################################

#set working directory 
#for Emily's computer: setwd("C:/Users/Vostro 3550/Desktop/Junior 18-19/Biocomputing/Biocomp_Folder/biocomputing_StatsGroupProject")

#load data 
antibiotics = read.csv(file="antibiotics.csv")

#plot the results using ggplot
library(ggplot2)
plotant = plot(antibiotics,ylab="Bacterial Growth",xlab="Treatment Type")
plotant

#make a new column for x1 and fill with 0,1 values
x1 = c(0) #create a vector for 0s
antibiotics$x1 = x1 #make the new x1 column and set as all 0s

x1.1 = c(1) #create a vector of 1s for the 1 values
antibiotics[5:8,3] = x1.1 #antibiotic treatment 1 is all 1 values, leaving all else as 0s

#make a new column for x2 and fill with 0,1 values
antibiotics$x2 = x1 #sets the entire column as 0s 
antibiotics[9:12,4] = x1.1 #antibiotic treatment 2 is all 1s in column x2, leaving all else as 0s

#make a new column for x3 and fill with 0,1 values
antibiotics$x3 = x1 #sets the entire column as 0s 
antibiotics[13:16,5] = x1.1 #antibiotic treatment 3 will be set as 1s, leaving all else as 0s

#create likelihood functions

#create the most complicated model 
ComplexMod<-function(p,x,y){
  #assign parameters
  B0=p[1]
  B1=p[2]
  B2=p[3]
  B3=p[4]
  sigma=exp(p[5])
  #unpack 
  x1 = x$x1
  x2 = x$x2 
  x3 = x$x3
  #define linear equation 
  pred=B0+B1*x1+B2*x2+B3*x3
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}

#create the simplest model
NullMod<-function(p,x,y){
  #assign parameters
  B0=p[1]
  sigma=exp(p[2])
  #unpack 
  x1 = x
  #equation
  pred=B0
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  
  return(nll)
}


# estimate parameters for each model- this is where it starts/the initial conditions
# these estimations are based on the averages shown from the plot generated above 
ComplexGuess=c(20,-15,-2,8,1)#estimations for most complicated ComplexMod
NullGuess = c(20,1) #estimations for simplest SimpleMod

#create fit for each model
fitComplex = optim(par=ComplexGuess,fn=ComplexMod,x = antibiotics[,3:5],y=antibiotics$growth)
fitNull=optim(par=NullGuess,fn=NullMod,x = antibiotics$x1,y=antibiotics$growth)

# run likelihood ratio tests 
# find statistical significance of the differences between the fit of the models

teststat=2*(fitNull$value-fitComplex$value) #determine test statistic value 

df=length(fitComplex$par)-length(fitNull$par) #determine degrees of freedom

1-pchisq(teststat,df) #chi square test for significance 

#p value that is given will show the degree of significance of the difference between the NullMod and the ComplexMod for the data