######################################
#######PART 2 OF STATS PROJECT########
######################################

###THE MONTE CARLO POWER ANALYSIS###


#remove global environment
rm(list=ls()) 
#set working directory using the setwd() function for your computer 


######create the models for regression, 2 Level ANOVA, 4 Level ANOVA, and 8 Level ANOVA #####

#create the null model 
NullMod = function(p,x,y) {
  #assign intercept parameter
  B0 = p[1]
  sigma = exp(p[2])
  
  #expected equation is just the intercept
  expected = B0
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll)
}

#create the regression model 
RegMod = function(p,x,y) {
  #assign parameters
  B0 = p[1]
  B1 = p[2]
  sigma = exp(p[3])
  
  #linear equation
  expected = B0 + B1*x
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return (nll)
}

#create the 2 Level ANOVA Model 

#create matrix for the 2 Level ANOVA Model x values  
matrix2 = matrix(0,24,1)
matrix2[13:24,1] = 1

#the 2 Level ANOVA model function
ANOVAx2Mod = function(p,x,y) {
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

#create the 4 Level ANOVA model 

#create 4 level anova matrix for x values 
matrix4 = matrix(0,24,3)
matrix4[7:12,1] = 1
matrix4[13:18,2] = 1
matrix4[19:24,3] = 1

ANOVAx4Mod = function(p,x,y) {
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

#create the 8 Level ANOVA model

#create the matrix for the x values 
matrix8 = matrix(0,24,7)
matrix8[4:6,1] = 1
matrix8[7:9,2] = 1
matrix8[10:12,3] = 1
matrix8[13:15,4] = 1
matrix8[16:18,5] = 1
matrix8[19:21,6] = 1
matrix8[22:24,7] = 1

#the 8 Level ANOVA model 
ANOVAx8Mod = function(p,x,y) {
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


######data simulation and for loops######
#data simulation
x = runif(24, min = 0, max = 50) #assigns 24 random values 0 to 50 along a uniform distribution to x

pval.reg = c() #create empty vector to later hold the p values of the regression 
pval.2A = c() #create another empty vector to later hold the p values of the 2 Level ANOVA
pval.4A = c()  #create another empty vector to later hold the p values of the 4 Level ANOVA
pval.8A =c() #create another empty vector to later hold the p values of the 8 Level ANOVA

y = list() #assign an empty list to y
out = list(y)#creates another list within this list named out

sigmas = c(1,2,4,6,8,12,16,24) #assigns sigma values to evaluate in the for loop 

#nested for loop 
for (j in 1:length(sigmas)) {
  for (i in 1:10) {
    error = rnorm(24,0,j) #creates error for a normal distribution with our data 
    y[[i]]= 10 + .4*x + error #linear equation of relationship between x and y 
    
    #guesses and fits for each model
    initialGuess.null = c(1,1) #initial guess for the null model
    fit.simple = optim(par = initialGuess.null, fn = NullMod, x = x ,y = y[[i]]) #optim fit for null model
    
    initialGuess.reg = c(1,1,1) #initial guess for 3 parameters for the regression
    fit.complex.reg = optim(par = initialGuess.reg, fn = RegMod,x=x, y=y[[i]]) #optim fit for regression model
    
    initialGuess.A2 = c(1,1,1) #initial guess  for 2 Level ANOVA
    fit.complex.A2 = optim(par = initialGuess.A2, fn = ANOVAx2Mod,x=x, y=y[[i]]) #optim fit for 2 Level ANOVA model
    
    initialGuess.A4 = c(1,1,1,1,1) #initial guess for 4 Level ANOVA
    fit.complex.A4 = optim(par = initialGuess.A4, fn = ANOVAx4Mod, x=x, y=y[[i]]) #optim fit for 4 Level ANOVA model
    
    initialGuess.A8 = c(5,5,5,5,5,5,5,5,1) #initial guess for 8 Level ANOVA
    fit.complex.A8 = optim(par = initialGuess.A8, fn = ANOVAx8Mod, x=x, y=y[[i]]) #optim fit for 8 Level ANOVA
    
    #test statistics and degrees of freedom for each model
    teststat.reg = 2*(fit.simple$value - fit.complex.reg$value) #for regression model, compute test statistic for chi-squared test
    df.reg = length(fit.complex.reg$par) - length(fit.simple$par) #for regression model, compute degrees of freedom for chi-squared test
    
    teststat.A2 = 2*(fit.simple$value - fit.complex.A2$value) #for 2L ANOVA, compute test statistic for chi-squared test
    df.A2 = length(fit.complex.A2$par) - length(fit.simple$par) #for 2L ANOVA, compute degrees of freedom for chi-squared test
    
    teststat.A4 = 2*(fit.simple$value - fit.complex.A4$value) #for 4L ANOVA, compute test statistic for chi-squared test
    df.A4 = length(fit.complex.A4$par) - length(fit.simple$par) #for 4L ANOVA, compute degrees of freedom for chi-squared test
    
    teststat.A8 = 2*(fit.simple$value - fit.complex.A8$value) #for 8L ANOVA, compute test statistic for chi-squared test
    df.A8 = length(fit.complex.A8$par) - length(fit.simple$par) #for 8L ANOVA, compute degrees of freedom for chi-squared test
    
    #bind the test statistics and degrees of freedom for each model into their respective empty vectors 
    pval.reg = rbind(pval.reg, pchisq(teststat.reg,df.reg, lower=F)) #for regression, bind the empty pval.reg vector and the chisquare results
    pval.2A = rbind(pval.2A, pchisq(teststat.A2,df.A2, lower=F))
    pval.4A = rbind(pval.4A, pchisq(teststat.A4,df.A4, lower=F))
    pval.8A = rbind(pval.8A, pchisq(teststat.A8,df.A8, lower=F))
  }
  out[[j]] = y
  y = list()
}

#group the sigmas for calculating their means
#for regressions
sig_1_mean.reg = mean(pval.reg[1:10])
sig_2_mean.reg = mean(pval.reg[11:20])
sig_4_mean.reg = mean(pval.reg[21:30])
sig_6_mean.reg = mean(pval.reg[31:40])
sig_8_mean.reg = mean(pval.reg[41:50])
sig_12_mean.reg = mean(pval.reg[51:60])
sig_16_mean.reg = mean(pval.reg[61:70])
sig_24_mean.reg = mean(pval.reg[71:80])
#create a vector with all the regression sigma means
Regression = c(sig_1_mean.reg,sig_2_mean.reg,sig_4_mean.reg,sig_6_mean.reg,sig_8_mean.reg,sig_12_mean.reg,sig_16_mean.reg,sig_24_mean.reg)

#for 2 Level ANOVAs
sig_1_mean.2A = mean(pval.2A[1:10])
sig_2_mean.2A = mean(pval.2A[11:20])
sig_4_mean.2A = mean(pval.2A[21:30])
sig_6_mean.2A = mean(pval.2A[31:40])
sig_8_mean.2A = mean(pval.2A[41:50])
sig_12_mean.2A = mean(pval.2A[51:60])
sig_16_mean.2A = mean(pval.2A[61:70])
sig_24_mean.2A = mean(pval.2A[71:80])
#create a vector with all the sigma means for the 2 Level ANOVA
ANOVA2 = rbind(sig_1_mean.2A,sig_2_mean.2A,sig_4_mean.2A,sig_6_mean.2A,sig_8_mean.2A,sig_12_mean.2A,sig_16_mean.2A,sig_24_mean.2A)

#for 4 Level ANOVAs
sig_1_mean.4A = mean(pval.4A[1:10])
sig_2_mean.4A = mean(pval.4A[11:20])
sig_4_mean.4A = mean(pval.4A[21:30])
sig_6_mean.4A = mean(pval.4A[31:40])
sig_8_mean.4A = mean(pval.4A[41:50])
sig_12_mean.4A = mean(pval.4A[51:60])
sig_16_mean.4A = mean(pval.4A[61:70])
sig_24_mean.4A = mean(pval.4A[71:80])
#create a vector with all the sigma means for the 4 Level ANOVA
ANOVA4 = rbind(sig_1_mean.4A,sig_2_mean.4A,sig_4_mean.4A,sig_6_mean.4A,sig_8_mean.4A,sig_12_mean.4A,sig_16_mean.4A,sig_24_mean.4A)

#for 8 Level ANOVAs
sig_1_mean.8A = mean(pval.8A[1:10])
sig_2_mean.8A = mean(pval.8A[11:20])
sig_4_mean.8A = mean(pval.8A[21:30])
sig_6_mean.8A = mean(pval.8A[31:40])
sig_8_mean.8A = mean(pval.8A[41:50])
sig_12_mean.8A = mean(pval.8A[51:60])
sig_16_mean.8A = mean(pval.8A[61:70])
sig_24_mean.8A = mean(pval.8A[71:80])
#create a vector with all the sigma means for the 8 Level ANOVA
ANOVA8 = rbind(sig_1_mean.8A,sig_2_mean.8A,sig_4_mean.8A,sig_6_mean.8A,sig_8_mean.8A,sig_12_mean.8A,sig_16_mean.8A,sig_24_mean.8A)
total = cbind(Regression, ANOVA2, ANOVA4, ANOVA8)
total
