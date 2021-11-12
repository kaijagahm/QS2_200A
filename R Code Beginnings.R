# load libraries
library(scales)
library(tidyverse)

# Demographic data (with variability among years) from paper:
s.A <- c(0.53, 0.68, 0.71, 0.38, 0.54, 0.69, 0.66, 0.49, 0.61) # adult survivorship
s.J <- c(0.56, 0.64, 0.3, 0.4, 0.38, 0.18, 0.25, 0.44) # juvenile survivorship
fecundity <- c(3.38, 1.27, 2.77, 2.17, 0.05, 4.0, 2.37, 0.5, 1.6, 2)


# Simulation setting
K <- 52 # carrying capacity--from the paper. Calculated from number of available territories
initial.pop <- K # the authors assume that the population basically sits at the carrying capacity
nyears <- 25 # the study happened over 10 years
n.I <- 0 # number of immigrants

### define the model ####

mod <- function(nyears, fecundity, s.A, s.J, samp = T, initial.pop = initial.pop){
  # set up data storage vectors
  years <- seq(1, nyears, 1) # create vector from 1:25
  population.data <- rep(NA, nyears) # going to store the population size for each year
  
  # define the initial population size
  population.data[1] <- initial.pop
  
  for(i in 2:nyears){
    current.pop <- population.data[i-1] # previous year's population size
    
    # calculate number of active territories:
    n.active <- floor(current.pop/2) # smallest whole number of territories
    
    # pick reproductive rate:
    current.fecundity <- ifelse(samp, sample(fecundity, size = 1), 
                                mean(fecundity))
    
    # calculate number of young fledged:
    n.fledged <- current.fecundity*n.active
    
    # calculate number of juveniles surviving the winter
    current.s.J <- ifelse(samp, sample(s.J, size = 1), mean(s.J))
    n.J <- floor(n.fledged*current.s.J)
    
    # calculate number of adults surviving the winter
    current.s.A <- ifelse(samp, sample(s.A, size = 1), mean(s.A))
    n.A <- floor((2*n.active)*current.s.A)
    
    # add up juveniles, adults, immigrants
    next.pop <- n.J + n.A + n.I
    
    # if this value is larger than carrying capacity, reduce population size accordingly:
    if(next.pop > K){
      next.pop <- K
    }
    
    # save result:
    population.data[i] <- next.pop
    
    if(next.pop < 2){
      break
    }
  }
  return(population.data)
}

run <- mod(nyears = nyears, fecundity = fecundity, s.A = s.A, s.J = s.J, 
            samp = T, initial.pop = initial.pop)

plot(run ~ years, type = 'l')

# That was great, but is only one possible outcome for this particular model.

#### Now run many simulations: ####

# To do this, we're going to use nested for() loops...

nyears <- 1000
nsims <- 1000
data <- matrix(NA, nrow = nyears, ncol = nsims)

# run the model 1000 times, storing each results vector in a new column of the `data` df
for(i in 1:nsims){
  popData <- mod(nyears = nyears, fecundity = fecundity, s.A = s.A, s.J = s.J, 
                 samp = T, initial.pop = initial.pop)
  data[,i] <- popData
}

head(data)

#### Visualize result: ####

# I made this plotting code into a function so we can use it over and over again.
plotSims <- function(data){
  plot(data[,1] ~ years,type='l',col=alpha('black',alpha=0.1),ylim=c(0,55),xlim=c(0,100))
  for(j in 2:nsims){
    lines(data[,j]~years,type='l',col=alpha('black',alpha=0.1))
  }
}

plotSims(data)

#### Figure out time to extinction for each simulation: ####
# Function to get extinction times
getExtinctionTimes <- function(data){
  extinction.times <- rep(NA, ncol(data))
  j <- 1
  for(j in 1:nsims){
    
    # make a temporary data set for a single simulated time series
    temporary <- data.frame(years,pops=data[,j])
    
    # drop all rows where population sizes are NA (ie, population extinct)
    temporary <- na.omit(temporary)  
    
    # The largest remaining year is the last year in which the population was extant
    extinction.times[j]<-max(temporary$years)
  }
  return(extinction.times)
}

et <- getExtinctionTimes(data)

# Figure #2 in Stacey & Taper 1992
hist(et, breaks = seq(1, max(et), 2))

median(et)

###################################

# Using the code above as a resource, address the following questions:

# 7. Can you replicate their assessment of the effects of variability? (in particular, what happens if you remove the variability? pg. 21). Take the model above, and replace the random draws of s.A, s.J, and fecundity with their mean values. Run the simulation again. How does the distribution of extinction times change?

dataMean <- matrix(NA, nrow = nyears, ncol = nsims)

# run the model 1000 times, storing each results vector in a new column of the `data` df
for(i in 1:nsims){
  popData <- mod(nyears = nyears, fecundity = fecundity, s.A = s.A, s.J = s.J, 
                 samp = F, # setting samp = F in the original function takes means instead of sampling from the vectors.
                 initial.pop = initial.pop)
  dataMean[,i] <- popData
}

head(dataMean)

#### Visualize result: ####
plotSims(dataMean) # all the same line!

#### Figure out time to extinction for each simulation: ####
et <- getExtinctionTimes(dataMean)

# Figure #2 in Stacey & Taper 1992
hist(et, breaks = seq(1, max(et), 2)) # all the same extinction time

median(et) # median of 27
table(et) # all 27.

# When we eliminate variability, all of the runs of the model return the same extinction time, but it's also higher than the median extinction time from the previous model (16-17 years). Now the (single) extinction time is 27 years. 

# 8. Pick out one of the assumptions of the paper, change it, and describe/illustrate what happens.

# Instead of assuming that the population basically sits at the carrying capacity, we're going to see what happens if the population starts at half the carrying capacity.

new.initial.pop <- K/2

# run the model 1000 times, storing each results vector in a new column of the `data` df
dataNew <- matrix(NA, nrow = nyears, ncol = nsims)
for(i in 1:nsims){
  popData <- mod(nyears = nyears, fecundity = fecundity, s.A = s.A, s.J = s.J, 
                 samp = T, initial.pop = new.initial.pop)
  dataNew[,i] <- popData
}

head(dataNew)

#### Visualize result: ####

plotSims(dataNew)

#### Figure out time to extinction for each simulation: ####
et <- getExtinctionTimes(dataNew)

# Figure #2 in Stacey & Taper 1992
hist(et, breaks = seq(1, max(et), 2))
median(et) # now the median is lower, at 12.
# The distribution is now more right-skewed than before, with a new median at 12 years instead of 16.

# 9. Is the basic simulation model actually completely density independent? hint: try calculating how many individuals are lost from the population each year (mortality + emigration), and produce a plot of this rate of loss against population density.

#The basic simulation model is not actually completely density independent because, as shown above, the population will still fluctuate and decrease around carrying capacity (K). 
 
# XXX need help figuring out if this code is right
dataLong <- as.data.frame(data) %>%
  mutate(year = 1:nrow(.)) %>% # add a column for the years explicitly
  pivot_longer(cols = -year, names_to = "sim", values_to = "popSize") %>% # now that we've pivoted to long format, each row is a unique year*simulation combo, with one value for population size. 
  arrange(sim) %>% # show the entire first simulation first, then the entire second simulation, etc. in order of years.
  # compute number of individuals lost in each year
  group_by(sim) %>% # have to group by which simulation run it is, because otherwise it will try to compare the last year of one simulation and the first year of the next simulation, which makes no sense.
  mutate(nLost = lag(popSize, 1) - popSize) %>% # subtract to get number lost
  mutate(nLost = case_when(nLost < 0 ~ 0, # if a negative number of individuals were lost, then none were lost--population grew in that year.
                           TRUE ~ nLost))

# Make a plot of the number of individuals lost vs. population density, across all simulations
dataLong %>%
  ggplot(aes(x = jitter(popSize), y = jitter(nLost)))+
  geom_point(alpha = 0.1, size = 0.7)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  ylab("# lost to death or emigration")+
  xlab("Population density")+
  ggtitle("Density-dependence of death/emigration")

# XXX It looks like the model is density-independent, or at least close? I'm not positive that I did this right.

# OPTIONAL:

# 10. What would happen if the sex-ratio of the population was not assumed to be 50:50? What are some different ways that this might occur? Can respond in writing, or supplement with quantitative results.

# 11. Can you figure out how Stacey & Taper calculated an overall population growth rate of lambda = 0.95 (on pg. 21)?

# 11. Did we actually reproduce the original model accurately? Fig. 2 suggests that the maximum time to extinction in their set of 1000 runs was 49 years. How often in sets of 1000 runs do we observe maximum extinction times longer than 49 years?






#### Remove effects of environmental variability: ####

# Only need to run the simulation once - there's nothing stochastic about it now

nyears<-1000
years<-seq(1,nyears,1)
population.data<-rep(NA,nyears)

population.data[1]<-initial.pop

#i<-2
for(i in 2:nyears){
  current.pop<-population.data[i-1]
  
  # calculate number of active territories:
  #n.active <- floor(current.pop/2)
  n.active <- round(current.pop/2)
  
  # pick reproductive rate:
  current.fecundity <- mean(fecundity)
  
  # calculate number of young fledged:
  n.fledged <- current.fecundity*n.active
  
  # calculate number of juveniles surviving the winter
  current.s.J <-  mean(s.J)
  #n.J <- floor(n.fledged*current.s.J)
  n.J <- round(n.fledged*current.s.J,digits = 1)
  
  # calculate number of adults surviving the winter
  current.s.A <- mean(s.A)
  #n.A <- floor((2*n.active)*current.s.A)
  n.A <- round((2*n.active)*current.s.A)
  
  # add up juveniles, adults, immigrants
  next.pop <- n.J + n.A + n.I
  
  # if this value is larger than carrying capacity, reduce population size accordingly:
  if(next.pop > K){
    next.pop <- K
  }
  
  # save result:
  population.data[i] <- next.pop
  
  if(next.pop < 2){
    break
  }
}

# visualize:
plot(population.data~years, type='l',xlim=c(0,30))

# Why doesn't this exactly match Stacey & Taper? ("each population lasted for 30 yr")  Maybe something to do with rounding.

bob<-population.data[2:length(population.data)]/population.data[1:I(length(population.data)-1)]
mean(bob[!is.na(bob)])
# 0.8756885

mean(s.J)
# 0.39375

mean(s.A)
# 0.5877778

mean(fecundity)
#2.011


# Consider empirically estimating population growth rate lambda:
j<-2
lambda<-rep(NA,nsims)
for(j in 1:nsims){
  temp <- data[,j]
  temp <- temp[!is.na(temp)]
  temp <- temp[temp!=0]
  
  lambda[j] <- mean(temp[2:length(temp)]/temp[1:I(length(temp)-1)])
}
lambda

hist(lambda)
mean(lambda)
median(lambda)

# somewhere around 0.83
