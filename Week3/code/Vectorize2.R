# Runs the stochastic Ricker equation with gaussian fluctuations

rm(list = ls())

stochrick <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2,numyears = 100)
{

  N <- matrix(NA, numyears, length(p0))  #initialize empty matrix

  N[1, ] <- p0

  for (pop in 1:length(p0)) { #loop through the populations

    for (yr in 2:numyears){ #for each pop, loop through the years

      N[yr, pop] <- N[yr-1, pop] * exp(r * (1 - N[yr - 1, pop] / K) + rnorm(1, 0, sigma)) # add one fluctuation from normal distribution
    
     }
  
  }
 return(N)

}

# Now write another function called stochrickvect that vectorizes the above to
# the extent possible, with improved performance: 

stochrickvect <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2, numyears = 100){
  # Initialisation
  N <- matrix(NA, numyears, length(p0))
  N[1, ] <- p0

  # Defining the function for looping through the years
  loop_years <- function(x){
    for (yr in 2:numyears){
      x[yr] <- x[yr-1] * exp(r * (1 - x[yr - 1] / K) + rnorm(1, 0, sigma))
    }
    return(x)
  }
  # Vectorising through the columns, which is the population
  N <- apply(N, 2, loop_years)
  
  return(N)
}


print("Vectorized Stochastic Ricker takes:")
print(system.time(res2<-stochrickvect()))