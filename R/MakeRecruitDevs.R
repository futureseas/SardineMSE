# Function to produce recruitment deviations for SSMSE::run_SSMSE
# Created: 7/30/2021, Robert Wildermuth

library(purrr)

MakeRecruitDevs <- function(envtInx = NULL, # numeric vector of either 0/1 regime indicators or covariate values (e.g., SST)
                            hiMean = NULL, # numeric mean of high recruitment regime
                            annChange = NULL, # scalar annual trend in mean recruitment
                            envtCoeff = NULL, # numeric coefficient of the effect of the environmental covariate on recruitment
                            nProj = NULL, # number of years for the OM projection
                            devSD # standard deviation of recruitment around mean
                            ){
  

  # Deviates based on regime indicator --------------------------------------
  
  if(!is.null(hiMean)){
    # Following mixture distribution from Maunder & Thorson 2019, pg 77
    # - this assumes the first (low) regime's mean is 0
    devs <- map_dbl(envtInx, function(x){ rnorm(1, mean = x * hiMean, sd = devSD)} ) 
    }
  
  # Deviates based on linear trend -------------------------------------------------
  
  if(!is.null(annChange)){
    devs <- 1:nProj
    devs <- map_dbl(devs, function(x){ rnorm(1, mean = x * annChange, sd = devSD)} )
    }
  
  # Deviates based on covariate ---------------------------------------------
  
  if(!is.null(envtCoeff)){
    # This is basically the same as the regime indicator calculation
    devs <- map_dbl(envtInx, function(x){ rnorm(1, mean = envtCoeff * x, sd = devSD)} )
    }
  
  # Correct for lognormal bias
  # !!RW: does this need to happen here, or is it handled elsewhere?
  # devs <- devs - 0.5 * devSD^2
  
  return(devs)
}


# Examples ----------------------------------------------------------------

# number of projection years
nProj <- 6

## Index-based recruitment devs
# index of regime 
envtInx <- as.numeric(rbernoulli(n = nProj, p = 0.5))
envtInx
# mean of high recruitment regime
hiMean <- 0.75

# Standard deviation of the recruitment deviations
devSD <- 1

MakeRecruitDevs(envtInx = envtInx, hiMean = hiMean, devSD = devSD)

## Trend in mean recruitment
# annual change in mean recruitment
annChange <- 0.05
1:nProj * annChange

MakeRecruitDevs(nProj = nProj, annChange = annChange, devSD = devSD)

## Recruitment deviations as a function of a covariate
# index of covariate
envtInx <- rnorm(nProj, sd = 2)
envtInx
# coefficient effect of environmental index on recruitment
envtCoeff <- 0.7

MakeRecruitDevs(envtInx = envtInx, envtCoeff = envtCoeff, devSD = devSD)
