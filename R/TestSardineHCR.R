library(tidyverse)
library(dplyr)
library(r4ss)

library(SSMSE)
# library(devtools)
# devtools::load_all(path = "C:/Users/rwildermuth/Documents/SSMSE")

# library(foreach) #if using run_parallel = TRUE
# library(doParallel) #if using run_parallel = TRUE

source("R/SourceDiagnosticPlots.R")

# directory for MSE output
mseOutputPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/margCompsOMfixedSelexEM"

# Operating Model - Research Model ----------------------------------------

# directory for OM SS code
OMmodelPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/OM/OM_20211019"
OMmodelPath <- "C:/Users/rwildermuth/Desktop/OM_K"
# RW: need to re-save data.ss_new as data.ss to fix formatting for SS_readdat()


# Define Observation Model ------------------------------------------------
# Run test of marginal comps OM
# datfile <- SS_readdat(file = paste0(OMmodelPath, "/filled_dat_marginals.ss"), version = "3.30")
datfile <- SS_readdat(file = paste0(OMmodelPath, "/dat.ss"), version = "3.30")
# create_sample_strct() has trouble IDing SE for survey CPUE
# define an index for the Acoustic-Trawl survey as in Desiree's code
#specify number of years of MSE loop
nyrs <- 10

#sample_struct <- create_sample_struct(dat = datfile, nyrs = nyrs)
#traceback()

#specify the start year of data inputs
yrsrt = datfile$endyr +1

#specify the end year of data inputs
yrend = datfile$endyr + nyrs

#sample_struct$CPUE = sample_struct$CPUE[1:nyrs,]
CPUE <- data.frame(Yr= yrsrt:yrend,
                   Seas= 1,
                   FltSvy = 4,
                   SE = 0.5)

#specify the number of catch fleets
ncdat=3

catch <- data.frame(Yr = rep(c(yrsrt:yrend),ncdat), 
                    Seas = c(rep(1,nyrs),rep(2,nyrs)),
                    FltSvy = c(rep(1,nyrs*2),rep(2,nyrs*2),rep(3,nyrs*2)),
                    SE = 0.05)
# # for now assume no additional age or length comps
# sample_struct$lencomp <- NULL
# sample_struct$agecomp <- NULL

#for length comps use AT summer survey (fleet #4 - Nsamp15), mexcal s1 (fleet #1 Nsamp 20), mexcal s2 (fleet #2-Nsamp 40),
#pnw (fleet #3 Nsamp 30 s1)
#the .dat file specifies month...but here it is labeled as season?
#specify the number of lengthcomp surveys
nldat=4
lencomp = data.frame(Yr = rep(c(yrsrt:yrend),nldat), 
                     Seas = c(rep(1,nyrs),rep(4,nyrs),rep(10,nyrs),rep(4,nyrs)),
                     FltSvy = c(rep(4,nyrs),rep(1,nyrs),rep(2,nyrs),rep(3,nyrs)),
                     Sex = rep(0,nyrs*nldat),
                     Part = rep(0,nyrs*nldat),
                     Nsamp = c(rep(20,nyrs),rep(20,nyrs),rep(20,nyrs),rep(20,nyrs)))

#for age comps same surveys as as lcomps
nadat=4
agecomp = data.frame(Yr = rep(c(yrsrt:yrend),nadat), 
                     Seas = c(rep(1,nyrs),rep(4,nyrs),rep(10,nyrs),rep(4,nyrs)),
                     FltSvy = c(rep(4,nyrs),rep(1,nyrs),rep(2,nyrs),rep(3,nyrs)),
                     Sex = rep(0,nyrs*nadat),
                     Part = rep(0,nyrs*nadat),
                     Ageerr = c(rep(4,nyrs),rep(4,nyrs),rep(4,nyrs),rep(4,nyrs)),
                     Lbin_lo = c(rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs)),
                     Lbin_hi = c(rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs)),
                     Nsamp = c(rep(20,nyrs),rep(20,nyrs),rep(20,nyrs),rep(20,nyrs)))

sample_struct <- list(catch = catch, CPUE = CPUE, lencomp = lencomp, agecomp = agecomp)
sample_struct_list <- list("SardineHCR" = sample_struct)
# Run the OM --------------------------------------------------------------

#run_res_path <- file.path("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE", "results")
# dir.create(mseOutputPath)

# EM starts in 1981 to test a high data quality scenario
EMmodelPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/EM/EM_alldat"
EMmodelPath <- "C:/Users/rwildermuth/Desktop/EM_K"
# EM starter.ss file must indicate init values are to be pulled from control.ss file, not ss.par

out <- run_SSMSE(scen_name_vec = "margComps_SardineHCR",# name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = c(1), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = OMmodelPath, # OM files
                  EM_name_vec = "margCompsOMfixedSelexEM", # cod is included in package data
                  EM_in_dir_vec = EMmodelPath, # EM files
                  # MS_vec = "no_catch",
                  MS_vec = "MS_sar_hcr",       # The management strategy is specified in the custom function
                  custom_MS_source = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/R/MS_sar_hcr.R", # file location of the MS function
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  # rec_dev_pattern = "rand", # Use random recruitment devs
                  # scope = "2", # to use the same recruitment devs across scenarios.
                  # impl_error_pattern = "none", # Don't use implementation error
                  # run_EM_last_yr = FALSE, # Run the EM in 106
                  run_parallel = FALSE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list, # How to sample data for running the EM.
                  seed = 12343) #Set a fixed integer seed that allows replication

# ~1.5 hrs for 5 its.

# Summarize results -------------------------------------------------------

# RW: runs failed for all iterations, but some went more than a few years.
#     Look at time series to diagnose

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(mseOutputPath, scenarios = "margComps_noCatch")

recrDiagPlots(dir = mseOutputPath,
              scenario = "margComps_SardineHCR", termYr = 2022)


compDiagPlots(dir = mseOutputPath,
              scenario = "margComps_SardineHCR",
              termYr = 2022)

age1plusDiagPlots(dir = mseOutputPath,
                  scenario = "margComps_SardineHCR", termYr = 2022)
