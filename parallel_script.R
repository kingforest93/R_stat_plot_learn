#clean workplace
rm(list=ls())
gc()

#self-defined function for test
func <- function(x){
n = 1
raw <- x
while(x>1){
x <- ifelse(x%%2==0,x/2,3*x+1)
n = n + 1
}
return(c(raw,n))
}

#single-core time taken
system.time({
x <- 1:1e5
lapply(x,func)
})

#multi-core time taken
library(parallel)
system.time({
x <- 1:1e5
cl <- makeCluster(8) #set how many processors used
results <- parLapply(cl, x, func) #parallel version of lapply
res.df <- do.call('rbind', results)
stopCluster(cl) #close the multi-core process
})
