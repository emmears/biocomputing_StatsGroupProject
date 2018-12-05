### REGRESSION SCRIPT
rm(list=ls()) #remove global environment
setwd("/Users/Ashley/Documents/Biocomputing_2018/biocomputing_StatsGroupProject")

sugar = read.csv("sugar.csv", header = TRUE)
sugar

library(ggplot2)

plot(sugar) #scatterplot of data points
abline(lm(growth~sugar, data = sugar))


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

nllike2 = function(p,x,y) {
  #assign intercept parameter
  B0 = p[1]
  sigma = exp(p[2])
  
  #expected equation is just the intercept
  expected = B0
  
  nll2 = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll2)
}

initialGuess = c(1,1,1) #initial guess for 3 parameters
fit.complex = optim(par = initialGuess, fn = nllike, x=sugar$sugar, y=sugar$growth)
print(fit.complex)

initialGuess2 = c(1,1) #intial guess for 2 parameters
fit.simple = optim(par = initialGuess2, fn = nllike2, x = sugar$sugar, y = sugar$growth)
print(fit.simple)

teststat = 2*(fit.simple$value - fit.complex$value) #compute test statistic for chi-squared test
df = length(fit.complex$par) - length(fit.simple$par) #compute degrees of freedom for chi-squared test
#check that degrees of freedom = 1
df

pchisq(teststat,df, lower=F) #run a chi-squared test to determine likelihood 
#2.638878e-10





