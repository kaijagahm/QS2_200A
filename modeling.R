# R code to construct a theta-logistic model
library(tidyverse) # please don't make me use base R
library(here)

# QUESTION 1: Make a theta-logistic model
# Starting parameters
r <- 0.2
k <- 10
nyears <- 20
theta <- 1
initial.pop <- 1

years <- seq(1, nyears, 1)
pops <- rep(NA, nyears) 
pops[1] <- initial.pop

for(i in 2:length(years)){ # loop through years
  starting <- pops[i-1] # starting is the population in the previous year
  new <- starting*exp(r*(1-(starting/k)^theta)) # new population for this year, which is the result of the theta-logistic equation (i.e. x(t+1) is 'new' here, and x(t) is 'starting' here.). We use the parameter values defined above.
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
thetaLogistic <- function(nyears = 20, initial.pop = 1, 
                          r = 0.2, k = 10, theta = 1){
  years <- seq(1, nyears, 1)
  pops <- rep(NA, nyears) 
  pops[1] <- initial.pop
  
  for(i in 2:length(years)){ # loop through years
    starting <- pops[i-1] # starting is the population in the previous year
    new <- starting*exp(r*(1-(starting/k)^theta)) # new population for this year, which is the result of the theta-logistic equation (i.e. x(t+1) is 'new' here, and x(t) is 'starting' here.). We use the parameter values defined above.
    pops[i] <- new
  }
  return(pops)
}

# QUESTION 2
# Trying values of theta between 0 and 5
thetas <- seq(0, 5, by = 1)

# plots
df <- data.frame(theta = paste0("theta = ", rep(thetas, each = nyears)),
                 year = rep(1:nyears, 6), 
                 pop = c(thetaLogistic(theta = thetas[1]),
                          thetaLogistic(theta = thetas[2]),
                          thetaLogistic(theta = thetas[3]),
                          thetaLogistic(theta = thetas[4]),
                          thetaLogistic(theta = thetas[5]),
                          thetaLogistic(theta = thetas[6])))

# Now let's make a plot
df %>%
  ggplot(aes(x = year, y = pop))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~theta)+
  xlab("Year since start")+
  ylab("Population size")+
  ggtitle("Theta-logistic population growth with varying thetas")

# As theta increases, the model's density-dependence gets stronger--the population growth slows down *more* with increasing density than it would have without the theta term. So when theta = 1, it's as if there was no theta exponent, so the model reduces to the basic Ricker model.

# QUESTION 4
allee <- function(a = 0.2, nyears = 10, initial.pop){
  # Restrict a to be between 0 and 1
  if(a < 0 | a > 1){
    stop("Parameter `a` must be between 0 and 1.")
  }
  
  years <- seq(1, nyears, 1)
  pops <- rep(NA, nyears) 
  pops[1] <- initial.pop
  
  for(i in 2:nyears){ # loop through years
    starting <- pops[i-1] # starting is the population in the previous year
    new <- starting + (1 - starting)*(starting - a) # calculate new population for this year
    if(new < 0){
      new <- 0
    }
    pops[i] <- new
  }
  return(pops) # the population size after nyears
}

# What happens when you vary the initial population size between 0 and 1?
initialPops <- seq(0, 1, by = 0.1)

df <- data.frame(initialPops = paste0("init pop size = ", rep(initialPops, each = nyears)),
                 year = rep(1:nyears, length(initialPops)), 
                 pop = c(allee(initial.pop = initialPops[1]),
                         allee(initial.pop = initialPops[2]),
                         allee(initial.pop = initialPops[3]),
                         allee(initial.pop = initialPops[4]),
                         allee(initial.pop = initialPops[5]),
                         allee(initial.pop = initialPops[6]),
                         allee(initial.pop = initialPops[7]),
                         allee(initial.pop = initialPops[8]),
                         allee(initial.pop = initialPops[9]),
                         allee(initial.pop = initialPops[10]),
                         allee(initial.pop = initialPops[11])))

# Now let's make a plot
p <- df %>%
  ggplot(aes(x = year, y = pop))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~initialPops, scales = "free_y")+
  xlab("Year since start")+
  ylab("Population size")

ggsave(p, file = here("allee.png"), width = 9, height = 5)

# When A = 0.2 and the initial population size is 0.5, the population goes extinct. In all other cases, the population grows, reaches 1, and stays there. In a few cases, the population fluctuates back down after reaching 1, but then it jumps back up to 1 again.
# We... are not quite sure we understand why this happens.

# QUESTION 5: What happens if you change the value of A?
plot(allee(initial.pop = 0.5, a = 1))
plot(allee(initial.pop = 0.5, a = 0.9)) # the population goes extinct when A is kind of high because the Allee effect is strong.
plot(allee(initial.pop = 0.5, a = 0.5))
plot(allee(initial.pop = 0.5, a = 0.1)) # the population doesn't go extinct when A is small because the Allee effect is weak.
