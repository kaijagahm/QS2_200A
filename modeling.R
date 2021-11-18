# R code to construct a theta-logistic model
library(tidyverse) # please don't make me use base R
library(here)

# Question 1 --------------------------------------------------------------
# Make a theta-logistic model
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


# Question 2 --------------------------------------------------------------
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

# As theta increases, the model's density-dependence is stronger--the population size slows down more with increasing density than it would have without the theta term.
# So when theta = 1, it's as if there were no theta exponent, so the model reduces to the Ricker model.
# In general, As theta increases, the point at which the population approaches their carrying capacity decreases.
# Populations with larger theta's will reach carrying capacity faster than those with smaller theta's.

# Question 3 --------------------------------------------------------------
# OPTIONAL: run your model again for theta = 10 and theta = 20, and plot time series of your population dynamics. What patterns do you observe? What do you think is happening in the model?
nyears <- 40 # going to run it over more years so we can see the longer-term dynamics.
newdf <- data.frame(theta = paste0("theta = ", 
                                   c(rep(10, nyears), rep(20, nyears))),
                year = rep(1:nyears, 2), 
                pop = c(thetaLogistic(theta = 10, nyears = 40),
                        thetaLogistic(theta = 20, nyears = 40)))
newdf %>%
  ggplot(aes(x = year, y = pop))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~theta)+
  xlab("Year since start")+
  ylab("Population size")+
  ggtitle("Theta-logistic population growth")

# In both of these cases, we see the population grow until it hits the carrying capacity, and then it fluctuates around that level (k = 10). When theta = 20, the population initially grows more slowly than when theta = 10, but not by much. 
# What's interesting is the pattern of fluctuations around the carrying capacity. When theta = 10, the population swings wildly around the carrying capacity, dropping down as low as 5 or 6 individuals before going back up to 12 or so. Over a long period of time, the swings don't seem to be dampening, they are just chaotic around the carrying capacity. When theta = 10, the swings are much smaller, and they seem to gradually dampen--it looks like the population is converging on the carrying capacity. 

# Question 4 --------------------------------------------------------------
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

# When A = 0.2 and the initial population size is 0.5, the population goes extinct. 
# In most other cases, the population reaches 1 and stays there. 
# With an initial population size of 1, the population seems to be stable for the forseeable future. 
# In a few cases, the population fluctuates back down after reaching 1, but then it jumps back up to 1 again.
# We... are not quite sure we understand why this happens.
# We are also wondering why at most A's the population drops around 10 years. 

# QUESTION 5: What happens if you change the value of A?
plot(allee(initial.pop = 0.5, a = 1)) #The population is stable at carrying capacity until year 10
plot(allee(initial.pop = 0.5, a = 0.9)) # the population goes extinct when A is kind of high ***<-Explain? I am not seeing a difference between 1 and 0.9-KH
plot(allee(initial.pop = 0.5, a = 0.5)) # the population is completely stable and flat. 
plot(allee(initial.pop = 0.5, a = 0.1)) # the population doesn't go extinct when A is small.Reaches carrying capacity.

# Question 6 --------------------------------------------------------------
#Consider what would happen if you add immigration to this model. Can you contrive a scenario where a population that would otherwise go extinct due to the Allee effect is able to persist due to immigration?

alleeWithImmigration <- function(a = 0.2, nyears = 10, initial.pop, imm){
  # Restrict a to be between 0 and 1
  if(a < 0 | a > 1){
    stop("Parameter `a` must be between 0 and 1.")
  }
  
  years <- seq(1, nyears, 1)
  pops <- rep(NA, nyears) 
  pops[1] <- initial.pop
  
  for(i in 2:nyears){ # loop through years
    starting <- pops[i-1] # starting is the population in the previous year
    new <- starting + (1 - starting)*(starting - a) + imm # calculate new population for this year
    pops[i] <- new
  }
  return(pops) # the population size after nyears
}
  
# Let's test some immigration rates and do a side-by-side comparison of the populations over time
imms <- 1:25 # test immigration rates from 1 to 25
init <- 100
nyears <- 40
plot(allee(a = 1, initial.pop = init, nyears = nyears))
# things are weird here--I can't figure out what's going on with my allee effects.
