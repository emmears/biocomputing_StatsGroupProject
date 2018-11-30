### REGRESSION SCRIPT
rm(list=ls()) #remove global environment
setwd("/Users/Ashley/Documents/Biocomputing_2018/biocomputing_StatsGroupProject")

sugar = read.csv("sugar.csv", header = TRUE)
sugar

library(ggplot2)

plot(sugar)

nllike = function(p,x,y) {
  B0 = p[1]
  B1 = p[2]
  sigma = exp(p[3])
  
  expected = B0 + B1*x
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return (nll)
}

nllike2 = function(p,x,y) {
  B0 = p[1]
  sigma = exp(p[2])
  
  expected = B0
  
  nll2 = -sum(x=y, mean = expected, sd = sigma, log = TRUE)
  return(nll2)
}

initialGuess = c(1,1,1)
fit.complex = optim(par = initialGuess, fn = nllike, x=sugar$sugar, y=sugar$growth)
print(fit.complex)

fit.simple = optim(par = initialGuess, fn = nllike2, x = sugar$sugar, y = sugar$growth)
print(fit.simple)

teststat = 2*(fit.simple$value - fit.complex$value)
df = length(fit.complex$par) - length(fit.simple$par)

pchisq(teststat,df, lower=F)

df
 


