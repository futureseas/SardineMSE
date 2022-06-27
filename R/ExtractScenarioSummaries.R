
library(tidyverse)
library(r4ss)

source("R/GetSumryOutput.R")
# directory for MSE output
dir <- "C:/Users/Robert W/Documents/FutureSeas/SSMSEscenarios/margCompsOMfixedSelexEM"
# scenario
scenario <- "margCompsOMfixedSelexEM_RandRecHCR1"
termYr <- 2068

RandRecHCR1 <- GetSumryOutput(dir = dir, scenario = scenario, termYr = termYr)
