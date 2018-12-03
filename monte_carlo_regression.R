#monte carlo regression
x = 1:50
sigmas = c(1,2,4,6,8,12,16,24)

y = list()
out = list(y)

for (j in sigmas) {
  for (i in 1:10) {
    error = rnorm(50,0,j)
    y[[i]]= 10 + .4*x + error
  }
  out[[j]] = y
  y = list()
}


  
  
  
  

