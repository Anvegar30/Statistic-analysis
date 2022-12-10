#Variables
library(ggplot2)
library(reshape2)
rm(list=ls())
paths <- 100
steps <- 250
dt <- 1/250
sigma <- 0.10
time <- 10 #years
total_steps <- time*steps - 1
total_paths <- paths
#Simulation
B <- matrix(nrow = total_steps+1, ncol = paths)

for (j in 1:total_paths) {
  B[1,j] = 0  #By definition of brownian motion
  for (i in 1:total_steps)
    
  {
    B[i+1,j] <- B[i,j] + sqrt(dt)*rnorm(1,mean=0,sd=sigma)
    
  }
}

B <- as.data.frame(B)

end = total_steps + 1
B$steps = seq(1:end)

d <- melt(B, id.vars="steps",variable.name = "series")

ggplot(d, aes(steps,value)) + geom_line(aes(colour = series)) +
  theme_classic()
