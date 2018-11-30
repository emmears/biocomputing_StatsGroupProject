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

initialGuess = c(1,1,1)
fit = optim(par = initialGuess, fn = nllike, x=sugar$sugar, y=sugar$growth)
print(fit)

teststat = fit$value
df = length(fit$par)

pt(teststat, df)


