#monte carlo ANOVA
#data simulation
x = runif(24, min = 0, max = 50) #assigns 24 random values 0 to 50 along a uniform distribution to x
sigmas = c(1,2,4,6,8,12,16,24) #assigns sigma values to evaluate

pval = c() #create empty vector to later hold the p values
pval2 = c() #create another empty vector to later hold the p values

y = list() #assign an empty list to y
out = list(y)#creates another list within this list named out 

#apply simulated data to equation
for (j in sigmas) {
  for (i in 1:10) {
    error = rnorm(24,0,j) #creates error for a normal distribution with our data 
    y[[i]]= 10 + .4*x + error #linear equation of relationship between x and y 
    
    initialGuess = c(1,1,1,1,1) #initial guess for 5 parameters
    fit.complex = optim(par = initialGuess, fn = nllike, x=x, y=y[[i]])
    
    initialGuess2 = c(1,1,1) #intial guess for 3 parameters
    fit.simple = optim(par = initialGuess2, fn = nllike2, x = x ,y = y[[i]])
    
    teststat = 2*(fit.simple$value - fit.complex$value) #compute test statistic for chi-squared test
    df = length(fit.complex$par) - length(fit.simple$par) #compute degrees of freedom for chi-squared test
    
    pval = rbind(pval, pchisq(teststat,df, lower=F)) #bind the empty pval vector and the chisquare results
  }
  out[[j]] = y
  y = list()
}

#create matrix 
matrix2 = matrix(0,24,1)
matrix2[13:24,1] = 1

ANOVA2Model = function(p,x,y) {
  #assign parameters
  B0 = p[1]
  B1 = p[2]
  sigma = exp(p[3])
  #unpack matrix 
  x1=matrix2[,1]
  #linear equation
  expected = B0 + B1*x1
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return (nll)
}

NullModel = function(p,x,y) {
  #assign intercept parameter
  B0 = p[1]
  sigma = exp(p[2])
  
  #expected equation is just the intercept
  expected = B0
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll)
}
#create 4 level anova matrix
matrix4 = matrix(0,24,3)
matrix4[7:12,1] = 1
matrix4[13:18,2] = 1
matrix4[19:24,3] = 1

ANOVA4Model = function(p,x,y) {
  #assign parameters
  B0 = p[1]
  B1 = p[2]
  B2 = p[3]
  B3 = p[4]
  sigma = exp(p[5])
  
  #unpack the matrix
  x1 = matrix4[,1]
  x2 = matrix4[,2]
  x3 = matrix4[,3]

  #create expected equation
  expected = B0 + B1*x1 + B2*x2 + B3*x3
  
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



