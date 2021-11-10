# R code to construct a theta-logistic model
library(tidyverse) # please don't make me use base R

# QUESTION 1: Make a theta-logistic model
# Starting parameters
r <- 1.2
k <- 100
nyears <- 20
theta <- 1
initial.pop <- 2

years <- seq(1, nyears, 1)
pops <- rep(NA, nyears) 
pops[1] <- initial.pop

for(i in 2:length(years)){ # loop through years
  starting <- pops[i-1] # starting pop is the population in the previous year
  new <- r*starting*(1-((starting/k)^theta)) # new population for this year is the result of the theta-logistic equation (i.e. x(t+1) is 'new' here, and x(t) is 'starting' here.). We use the parameter values defined above.
  pops[i] <- new
}

# plot the results:
df <- data.frame(year = years,
                 pop = pops)
df %>%
  ggplot(aes(x = year, y = pop))+
  geom_point()+
  geom_line()+
  ylab("Population size")+
  xlab("Year from start")+
  theme_classic()+
  ggtitle("Theta-logistic population growth over time")

# going to make this into a function so we don't have to re-write it every time
thetaLogistic <- function(nyears = 20, initial.pop = 2, 
                          r = 1.2, k = 100, theta = 1){
  years <- seq(1, nyears, 1)
  pops <- rep(NA, nyears) 
  pops[1] <- initial.pop
  
  for(i in 2:length(years)){ # loop through years
    starting <- pops[i-1] # starting pop is the population in the previous year
    new <- r*starting*(1-((starting/k)^theta)) # new population for this year is the result of the theta-logistic equation (i.e. x(t+1) is 'new' here, and x(t) is 'starting' here.). We use the parameter values defined above.
    pops[i] <- new
  }
  return(pops)
}

# QUESTION 2
# trying three different values of theta: let's try -1, 0, and 0.5
theta1 <- 22
theta2 <- 0
theta3 <- 0.5

pops1 <- thetaLogistic(theta = theta1)
pops2 <- thetaLogistic(theta = theta2)
pops3 <- thetaLogistic(theta = theta3)

# plots
df <- data.frame(year = rep(years, 3),
                 pop = c(pops1, pops2, pops3),
                 theta = c(rep(theta1, length(pops1)), 
                           rep(theta2, length(pops2)), 
                           rep(theta3, length(pops3)))
)

# Now let's make a plot
df %>%
  ggplot(aes(x = year, y = pop))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~theta, scales = "free_y")+
  xlab("Year since start")+
  ylab("Population size")

# QUESTION 3
allee <- function(a = 5, nyears = 30, initial.pop, r = 0.5, k = 100){
  years <- seq(1, nyears, 1)
  pops <- rep(NA, nyears) 
  pops[1] <- initial.pop
  
  for(i in 2:nyears){ # loop through years
    starting <- pops[i-1] # starting pop is the population in the previous year
    new <- r*starting*((starting/a)-1)*(1-(starting/k)) # calculate new population for this year
    pops[i] <- new
  }
  return(last(pops)) # the population size after nyears
}

# Now let's vary the initial population size
initialPops <- seq(0, 70, by = 1)

finalSizes <- rep(NA, length(initialPops))

for(i in 1:length(initialPops)){
  finalSizes[i] <- allee(initial.pop = initialPops[i])
}

plot(finalSizes)

