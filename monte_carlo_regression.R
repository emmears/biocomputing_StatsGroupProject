#monte carlo regression
x = runif(24, min = 0, max = 50)
sigmas = c(1,2,4,6,8,12,16,24)

pval = c()

y = list()
out = list(y)

for (j in sigmas) {
  for (i in 1:10) {
    error = rnorm(24,0,j)
    y[[i]]= 10 + .4*x + error
    
    initialGuess = c(1,1,1) #initial guess for 3 parameters
    fit.complex = optim(par = initialGuess, fn = nllike, x=x, y=y[[i]])
    
    initialGuess2 = c(1,1) #intial guess fro 2 parameters
    fit.simple = optim(par = initialGuess2, fn = nllike2, x = x ,y = y[[i]])
    
    teststat = 2*(fit.simple$value - fit.complex$value) #compute test statistic for chi-squared test
    df = length(fit.complex$par) - length(fit.simple$par) #compute degrees of freedom for chi-squared test
    
    pval = rbind(pval, pchisq(teststat,df, lower=F))
  }
  out[[j]] = y
  y = list()
}


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

sig_1_mean = mean(pval[1:10])
sig_2_mean = mean(pval[11:20])
sig_4_mean = mean(pval[21:30])
sig_6_mean = mean(pval[31:40])
sig_8_mean = mean(pval[41:50])
sig_12_mean = mean(pval[51:60])
sig_16_mean = mean(pval[61:70])
sig_24_mean = mean(pval[71:80])

sig_1_mean
sig_2_mean
sig_4_mean
sig_6_mean
sig_8_mean
sig_12_mean
sig_16_mean
sig_24_mean
  

