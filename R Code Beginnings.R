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

years <- seq(1, nyears, 1) # create vector from 1:25

### define the model ####

mod <- function(nyears, fecundity, s.A, s.J, samp = T, initial.pop = initial.pop){
  # set up data storage vectors
  years <- seq(1, nyears, 1)
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

years<-seq(1,nyears,1)

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
  plot(data[,1] ~ years,ylab="Population size", xlab="Years", main="Simulations of population growth over time",
  type='l',col=alpha('black',alpha=0.1),ylim=c(0,55),xlim=c(0,100))
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
hist(et, breaks = seq(1, max(et), 2), main= "Time to extinction", xlab="Years")

median(et)

###################################

# Using the code above as a resource, address the following questions:

# 7. Can you replicate their assessment of the effects of variability? (in particular, what happens if you remove the variability? pg. 21). Take the model above, and replace the random draws of s.A, s.J, and fecundity with their mean values. Run the simulation again. How does the distribution of extinction times change?

# Yes, when variability is included, we can replicate their assessment and plot Figure 2 from Stacey & Taper 1992.
# When we eliminate variability, all of the runs of the model return the same extinction time, but it's also higher than the median extinction time from the previous model with variability (16-17 years). Now the (single) extinction time is 27 years. 

dataMean <- matrix(NA, nrow = nyears, ncol = nsims)

# run the model 1000 times, storing each results vector in a new column of the `data` df
for(i in 1:nsims){
  popData <- mod(nyears = nyears, fecundity = fecundity, s.A = s.A, s.J = s.J, 
                 samp = F, # setting samp = F in the original function takes means instead of sampling from the vectors.
                 initial.pop = initial.pop)
  dataMean[,i] <- popData
}

head(dataMean)

# Visualize results:
plotSims(dataMean) # all the same line!

#Figure out time to extinction for each simulation:
et <- getExtinctionTimes(dataMean)
hist(et, breaks = seq(1, max(et), 2), main= "Time to extinction", xlab="Years") # all the same extinction time
median(et) # median of 27
table(et) # all 27.


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

#Visualize result:

plotSims(dataNew)

#Figure out time to extinction for each simulation:
et <- getExtinctionTimes(dataNew)
hist(et, breaks = seq(1, max(et), 2), main= "Time to extinction", xlab="Years")
median(et) # now the median is lower, at 12.

# When the population starts at half its carrying capacity, its time to extinction becomes shorter.
# The distribution is now more right-skewed than before, with a new median at 12 years instead of 16.


# 9. Is the basic simulation model actually completely density independent? hint: try calculating how many individuals are lost from the population each year (mortality + emigration), and produce a plot of this rate of loss against population density.

#The basic simulation model is not actually completely density independent because, as shown above, the population will still fluctuate and decrease around carrying capacity (K). 
#However, across large time spans (or many simulations over 1000 years in following case), density becomes negligible. 
 
#Calculating individuals lost due to death and emigration across all simulations:
dataLong <- as.data.frame(data) %>%
  mutate(year = 1:nrow(.)) %>% # add a column for the years explicitly
  pivot_longer(cols = -year, names_to = "sim", values_to = "popSize") %>% # now that we've pivoted to long format, each row is a unique year*simulation combo, with one value for population size. 
  arrange(sim) %>% # show the entire first simulation first, then the entire second simulation, etc. in order of years.
  # compute number of individuals lost in each year
  group_by(sim) %>% # have to group by which simulation run it is, because otherwise it will try to compare the last year of one simulation and the first year of the next simulation, which makes no sense.
  mutate(nLost = lag(popSize, 1) - popSize) %>% # subtract to get number lost
  mutate(nLost = case_when(nLost < 0 ~ 0, # if a negative number of individuals were lost, then none were lost--population grew in that year.
                           TRUE ~ nLost))

# Make a plot of the number of individuals lost vs. population density, across all simulations:
dataLong %>%
  ggplot(aes(x = jitter(popSize), y = jitter(nLost)))+
  geom_point(alpha = 0.1, size = 0.7)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  ylab("Individuals lost to death or emigration")+
  xlab("Population density")+
  ggtitle("Density-dependence of death/emigration")


##### XXX It looks like the model is density-independent, or at least close? I'm not positive that I did this right.

##### XXX Stella's comments: I believe you are correct, both in the calculation and plotting. I think the issue lies with the very high number of simulations and years.
##### When nyears and nsims are both 1000, I think it's so high that the model becomes basically density-independent. If you play around with nyears and nsims
##### you definitely see density-dependence of some sort. See following:
nyears <- 10
nsims <- 1000
data <- matrix(NA, nrow = nyears, ncol = nsims)

years<-seq(1,nyears,1)

# run the model 1000 times, storing each results vector in a new column of the `data` df
for(i in 1:nsims){
  popData <- mod(nyears = nyears, fecundity = fecundity, s.A = s.A, s.J = s.J, 
                 samp = T, initial.pop = initial.pop)
  data[,i] <- popData
}

#Calculating individuals lost due to death and emigration across all simulations:
dataLong <- as.data.frame(data) %>%
  mutate(year = 1:nrow(.)) %>% # add a column for the years explicitly
  pivot_longer(cols = -year, names_to = "sim", values_to = "popSize") %>% # now that we've pivoted to long format, each row is a unique year*simulation combo, with one value for population size. 
  arrange(sim) %>% # show the entire first simulation first, then the entire second simulation, etc. in order of years.
  # compute number of individuals lost in each year
  group_by(sim) %>% # have to group by which simulation run it is, because otherwise it will try to compare the last year of one simulation and the first year of the next simulation, which makes no sense.
  mutate(nLost = lag(popSize, 1) - popSize) %>% # subtract to get number lost
  mutate(nLost = case_when(nLost < 0 ~ 0, # if a negative number of individuals were lost, then none were lost--population grew in that year.
                           TRUE ~ nLost))

# Make a plot of the number of individuals lost vs. population density, across all simulations:
dataLong %>%
  ggplot(aes(x = jitter(popSize), y = jitter(nLost)))+
  geom_point(alpha = 0.1, size = 0.7)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  ylab("Individuals lost to death or emigration")+
  xlab("Population density")+
  ggtitle("Density-dependence of death/emigration")
