######################################
#######PART 1 OF STATS PROJECT########
######################################

#####Question 1: The ANOVA#####
#A student conducted an experiment evaluating the effect of three different new antibiotics 
#on growth of E. coli in lab cultures. Using the data in antibiotics.txt, 
#generate a plot that summarizes the results and test for an effect of antibiotic treatments 
#on the growth of E. coli using an ANOVA-design linear model and likelihood ratio test. 

#remove global environment
rm(list=ls()) 
#set working directory using the setwd() function for your computer 

#load data 
antibiotics = read.csv(file="antibiotics.csv")
antibiotics #check out the data

#plot the results as a bow and whisker plot using ggplot
library(ggplot2)
plotant = plot(antibiotics,ylab="Bacterial Growth",xlab="Treatment Type")
plotant

#To begin the ANOVA analysis, distinguish between treatments by 0,1 pairings
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

#create likelihood functions for the ANOVA model

#create the most complicated model 
ANOVAMod<-function(p,x,y){
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
# these estimations are based on the medians shown from the plot generated above 
ANOVAGuess=c(20,-15,-2,8,1)#estimations for most complicated ComplexMod
NullGuess = c(20,1) #estimations for simplest SimpleMod

#create fit for each model
fitANOVA = optim(par=ANOVAGuess,fn=ANOVAMod,x = antibiotics[,3:5],y=antibiotics$growth)
fitNull=optim(par=NullGuess,fn=NullMod,x = antibiotics$x1,y=antibiotics$growth)

# run likelihood ratio tests 
# find statistical significance of the differences between the fit of the models

teststat.anova=2*(fitNull$value-fitANOVA$value) #determine test statistic value 
teststat.anova #37.90132
df.anova=length(fitANOVA$par)-length(fitNull$par) #determine degrees of freedom 
df.anova #3

1-pchisq(teststat.anova,df.anova) #chi square test for significance 
#returns a value of 2.96576 E -08
#p value that is given will show the degree of significance of the difference between the NullMod and the ANOVAMod for the data


###### Question 2: The Regression ######
#Another student conducted an experiment evaluating the effect of sugar concentration 
#on growth of E. coli in lab cultures. Using the data in sugar.txt, generate a plot 
#that summarizes the results and test for an effect of sugar concentration on growth 
#of E. coli using a regression-design linear model and likelihood ratio test. 

#read in the data 
sugar = read.csv("sugar.csv", header = TRUE)
sugar

#create a scatterplot of data points
library(ggplot2)
plot(sugar) #the plot
abline(lm(growth~sugar, data = sugar)) #adding the regression line based on the linear model of the data


#create likelihood functions for the regression Model

#create the most complex model
nllike = function(p,x,y) {
  #assign parameters
  B0 = p[1]
  B1 = p[2]
  sigma = exp(p[3])
  
  #linear equation
  expected = B0 + B1*x
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return (nll)
}

#create the null, simplest model 
nllike2 = function(p,x,y) {
  #assign intercept parameter
  B0 = p[1]
  sigma = exp(p[2])
  
  #expected equation is just the intercept
  expected = B0
  
  nll2 = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll2)
}

#estimate parameters for each model 
initialGuess = c(1,1,1) #initial guess for 3 parameters
fit.complex = optim(par = initialGuess, fn = nllike, x=sugar$sugar, y=sugar$growth) #create the optimized fit for the complex model 
print(fit.complex) #print the fit

initialGuess2 = c(1,1) #intial guess for 2 parameters
fit.simple = optim(par = initialGuess2, fn = nllike2, x = sugar$sugar, y = sugar$growth) #create the optimized fit for the simple null model
print(fit.simple) #print the fit 

#test for significance 
teststat.reg = 2*(fit.simple$value - fit.complex$value) #compute test statistic for chi-squared test
teststat.reg #teststat.reg = 39.92512
df.reg = length(fit.complex$par) - length(fit.simple$par) #compute degrees of freedom for chi-squared test
#check that degrees of freedom = 1
df.reg #df.reg =1

pchisq(teststat.reg,df.reg, lower=F) #run a chi-squared test to determine likelihood 
#2.638878e-10

#p value that is given will show the degree of significance of the difference between the Null nllike2 model and the regression nllike model for the data
