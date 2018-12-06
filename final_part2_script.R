######################################
#######PART 2 OF STATS PROJECT########
######################################

###THE MONTE CARLO POWER ANALYSIS###


#remove global environment
rm(list=ls()) 
#set working directory using the setwd() function for your computer 


######create the models for regression, 2 Level ANOVA, 4 Level ANOVA, and 8 Level ANOVA #####

#create the null model 
NullModel = function(p,x,y) {
  #assign intercept parameter
  B0 = p[1]
  sigma = exp(p[2])
  
  #expected equation is just the intercept
  expected = B0
  
  nll = -sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return(nll)
}

#create the regression model 
RegModel = function(p,x,y) {
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

