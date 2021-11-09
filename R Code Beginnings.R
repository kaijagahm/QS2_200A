# load libraries
library(scales)

# Demographic data (with variability among years) from paper:
s.A <- c(0.53, 0.68, 0.71, 0.38, 0.54, 0.69, 0.66, 0.49, 0.61) # adult survivorship
s.J <- c(0.56, 0.64, 0.3, 0.4, 0.38, 0.18, 0.25, 0.44) # juvenile survivorship
fecundity <- c(3.38, 1.27, 2.77, 2.17, 0.05, 4.0, 2.37, 0.5, 1.6, 2)


# Simulation setting
K <- 52 # carrying capacity--from the paper. Calculated from number of available territories
initial.pop <- K # the authors assume that the population basically sits at the carrying capacity
nyears <- 25 # the study happened over 10 years
n.I <- 0 # number of immigrants

# set up data storage vectors
years <- seq(1, nyears, 1) # create vector from 1:25
population.data <- rep(NA, nyears) # going to store the population size for each year

# define the initial population size
population.data[1] <- initial.pop


### Run the model! ####

#i<-2
for(i in 2:nyears){
  current.pop <- population.data[i-1] # previous year's population size
  
  # calculate number of active territories:
  n.active <- floor(current.pop/2) # smallest whole number of territories
  
  # pick reproductive rate:
  current.fecundity <- sample(fecundity, size = 1) # pick a reproductive rate at random from the ones observed
  
  # calculate number of young fledged:
  n.fledged <- current.fecundity*n.active
  
  # calculate number of juveniles surviving the winter
  current.s.J <-  sample(s.J, size = 1)
  n.J <- floor(n.fledged*current.s.J)
  
  # calculate number of adults surviving the winter
  current.s.A <- sample(s.A, size = 1)
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

plot(population.data~years, type='l')


# That was great, but is only one possible outcome for this particular model.



#### Now run many simulations: ####

# To do this, we're going to use nested for() loops...

nyears<-1000
nsims<-1000
data<-matrix(NA,nrow=nyears,ncol=nsims)

years<-seq(1,nyears,1)

for(j in 1:nsims){
  
  population.data<-rep(NA,nyears)
  
  population.data[1]<-initial.pop
  
  #i<-2
  for(i in 2:nyears){
    current.pop<-population.data[i-1]
    
    # calculate number of active territories:
    n.active <- floor(current.pop/2)
    
    # pick reproductive rate:
    current.fecundity <- sample(fecundity, size = 1)
    
    # calculate number of young fledged:
    n.fledged <- current.fecundity*n.active
    
    # calculate number of juveniles surviving the winter
    current.s.J <-  sample(s.J, size = 1)
    n.J <- floor(n.fledged*current.s.J)
    
    # calculate number of adults surviving the winter
    current.s.A <- sample(s.A, size = 1)
    n.A <- floor((2*n.active)*current.s.A)
    
    # add up juveniles, adults, immigrants
    next.pop <- n.J + n.A + n.I
    
    # if this value is larger than carrying capacity, reduce population size accordingly:
    if(next.pop > K){
      next.pop <- K
    }
    
    # save this result:
    population.data[i] <- next.pop
    
    # if population size has dropped below 2, end the simulation
    if(next.pop < 2){
      break
    }
  }
  
  data[,j]<-population.data
}

head(data)


#### Visualize result: ####

plot(data[,1]~years,type='l',col=alpha('black',alpha=0.1),ylim=c(0,55),xlim=c(0,100))
for(j in 2:nsims){
  lines(data[,j]~years,type='l',col=alpha('black',alpha=0.1))
}


#### Figure out time to extinction for each simulation: ####
extinction.times<-rep(NA,nsims)
j<-1
for(j in 1:nsims){
  
  # make a temporary data set for a single simulated time series
  temporary <- data.frame(years,pops=data[,j])
  
  # drop all rows where population sizes are NA (ie, population extinct)
  temporary <- na.omit(temporary)  
  
  # The largest remaining year is the last year in which the population was extant
  extinction.times[j]<-max(temporary$years)
}

# Figure #2 in Stacey & Taper 1992
hist(extinction.times,breaks = seq(1,61,2))

median(extinction.times)


###################################

# Using the code above as a resource, address the following questions:

# 6. Can you replicate their assessment of the effects of variability? (in particular, what happens if you remove the variability? pg. 21). Take the model above, and replace the random draws of s.A, s.J, and fecundity with their mean values. Run the simulation again. How does the distribution of extinction times change?

# 7. Pick out one of the assumptions of the paper, change it, and describe/illustrate what happens.

# 8. Is the basic simulation model actually completely density independent? hint: try calculating how many individuals are lost from the population each year (mortality + emigration), and produce a plot of this rate of loss against population density.

# OPTIONAL:

# 9. What would happen if the sex-ratio of the population was not assumed to be 50:50? What are some different ways that this might occur? Can respond in writing, or supplement with quantitative results.

# 10. Can you figure out how Stacey & Taper calculated an overall population growth rate of lambda = 0.95 (on pg. 21)?

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
