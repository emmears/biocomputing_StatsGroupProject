#monte carlo regression
x = runif(24, min = 0, max = 50)
sigmas = c(1,2,4,6,8,12,16,24)

pval = c()
pval2 = c()

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
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll)
}

matrix8 = matrix(0,24,7)
matrix8[4:6,1] = 1
matrix8[7:9,2] = 1
matrix8[10:12,3] = 1
matrix8[13:15,4] = 1
matrix8[16:18,5] = 1
matrix8[19:21,6] = 1
matrix8[22:24,7] = 1

nllike_anova = function(p,x,y) {
  #assign parameters
  B0 = p[1]
  B1 = p[2]
  B2 = p[3]
  B3 = p[4]
  B4 = p[5]
  B5 = p[6]
  B6 = p[7]
  B7 = p[8]
  sigma = exp(p[9])
  
  #unpack the matrix
  x1 = matrix8[,1]
  x2 = matrix8[,2]
  x3 = matrix8[,3]
  x4 = matrix8[,4]
  x5 = matrix8[,5]
  x6 = matrix8[,6]
  x7 = matrix8[,7]
  
  #create expected equation
  expected = B0 + B1*x1 + B2*x2 + B3*x3 + B4*x4 + B5*x5 + B6*x6 + B7*x7
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll)
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

#8-LEVEL ANOVA

for (j in sigmas) {
  for(i in 1:10) {
    error = rnorm(24,0,j)
    y[[i]]= 10 + .4*x + error
    
    initialGuess = c(1,1,1,1,1,1,1,1,1) #initial guess for 9 parameters
    fit.complex = optim(par = initialGuess, fn = nllike_anova, x=x, y=y[[i]])
  
    initialGuess2 = c(1,1) #intial guess fro 2 parameters
    fit.simple = optim(par = initialGuess2, fn = nllike2, x = x ,y = y[[i]])
    
    teststat = 2*(fit.simple$value - fit.complex$value) #compute test statistic for chi-squared test
    df = length(fit.complex$par) - length(fit.simple$par) #compute degrees of freedom for chi-squared test
    
    pval2 = rbind(pval2, pchisq(teststat,df, lower=F))
  }
  out[[j]] = y
  y = list()
}

pval2
  

