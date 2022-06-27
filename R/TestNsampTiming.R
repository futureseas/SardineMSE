
library(tidyverse)
library(SSMSE)
library(r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE

source("MakeRecruitDevs.R")
source("R/SourceDiagnosticPlots.R")


# define SD of recruitment deviations as 'sigmaR' from SS control file
devSD <- 1

#specify number of years of MSE loop
nyrs = 10

# Test 5% increase in recruitment -----------------------------------------

annChangDevs <- MakeRecruitDevs(nProj = nyrs, annChange = 0.05, devSD = devSD)

# directory for MSE output
mseOutputPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM"

# Operating Model - Research Model ----------------------------------------

# directory for OM SS code
OMmodelPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/0_estimate_cohort_growh"

# Define Observation Model ------------------------------------------------
datfile <- SS_readdat(file = paste0(OMmodelPath, "/filled_dat_marginals.ss"), version = "3.30")
sample_struct <- create_sample_struct(dat = datfile, nyrs = nyrs)

# create_sample_strct() has trouble IDing SE for survey CPUE
# define an index for the Acoustic-Trawl survey as in Desiree's code

#specify the start year of data inputs
yrsrt = datfile$endyr +1

#specify the end year of data inputs
yrend = datfile$endyr + nyrs

sample_struct$CPUE = sample_struct$CPUE[1:nyrs,]
sample_struct$CPUE$FltSvy = 4
sample_struct$CPUE$SE = 0.5
sample_struct$CPUE$Yr= yrsrt:yrend
sample_struct$CPUE$Seas= 1

# # for now assume no additional age or length comps
# sample_struct$lencomp <- NULL
# sample_struct$agecomp <- NULL

#for length comps use AT summer survey (fleet #4 - Nsamp15), mexcal s1 (fleet #1 Nsamp 20), mexcal s2 (fleet #2-Nsamp 40),
#pnw (fleet #3 Nsamp 30 s1)
#the .dat file specifies month...but here it is labeled as season?
#specify the number of lengthcomp surveys
nldat=4
sample_struct$lencomp = data.frame(Yr = rep(c(yrsrt:yrend),nldat), 
                                   Seas = c(rep(1,nyrs),rep(4,nyrs),rep(10,nyrs),rep(4,nyrs)),
                                   FltSvy = c(rep(4,nyrs),rep(1,nyrs),rep(2,nyrs),rep(3,nyrs)),
                                   Sex = rep(0,nyrs*nldat),
                                   Part = rep(0,nyrs*nldat),
                                   Nsamp = c(rep(100,nyrs),rep(100,nyrs),rep(100,nyrs),rep(100,nyrs)))

#for age comps same surveys as as lcomps
nadat=4
sample_struct$agecomp = data.frame(Yr = rep(c(yrsrt:yrend),nadat), 
                                   Seas = c(rep(1,nyrs),rep(4,nyrs),rep(10,nyrs),rep(4,nyrs)),
                                   FltSvy = c(rep(4,nyrs),rep(1,nyrs),rep(2,nyrs),rep(3,nyrs)),
                                   Sex = rep(0,nyrs*nadat),
                                   Part = rep(0,nyrs*nadat),
                                   Ageerr = c(rep(4,nyrs),rep(4,nyrs),rep(4,nyrs),rep(4,nyrs)),
                                   Lbin_lo = c(rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs)),
                                   Lbin_hi = c(rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs)),
                                   Nsamp = c(rep(100,nyrs),rep(100,nyrs),rep(100,nyrs),rep(100,nyrs)))


sample_struct_list <- list("5pctAnnChangeNsamp10Model" = sample_struct)
# Run the OM --------------------------------------------------------------

#run_res_path <- file.path("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE", "results")
# dir.create(mseOutputPath)

# EM starts in 1981 to check high data quality again
EMmodelPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/EM/4_shortEM_constgrowth_constselex"
# EM starter.ss file must indicate init values are to be pulled from control.ss file, not ss.par

timeStart <- Sys.time()

run_SSMSE(scen_name_vec = "5pctAnnChangeNsamp10Model",# name of the scenario
          out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
          iter_vec = c(10), # run with 5 iterations for now
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = OMmodelPath, # OM files
          EM_name_vec = "constGrowthfixedParamsEM", # cod is included in package data
          EM_in_dir_vec = EMmodelPath, # EM files
          MS_vec = "EM",       # The management strategy is specified in the EM
          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
          nyrs_vec = nyrs,        # Years to project OM forward
          nyrs_assess_vec = 1, # Years between assessments
          rec_dev_pattern = "vector", # Use user-defined recruitment devs
          rec_dev_pars = t(annChangDevs), # user-provided recruitment devs
          scope = "2", # to use the same recruitment devs across scenarios.
          impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = TRUE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 1234) #Set a fixed integer seed that allows replication

timeEnd <- Sys.time()

print(paste("Time for Nsamp = 10, its = 10, nyrs = 10:", timeEnd - timeStart))


# Test Nsamp = 20 ---------------------------------------------------------

sample_struct$agecomp$Nsamp <- 20

sample_struct$lencomp$Nsamp <- 20

sample_struct_list <- list("5pctAnnChangeNsamp20Model" = sample_struct)

timeStart <- Sys.time()

run_SSMSE(scen_name_vec = "5pctAnnChangeNsamp20Model",# name of the scenario
          out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
          iter_vec = c(10), # run with 5 iterations for now
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = OMmodelPath, # OM files
          EM_name_vec = "constGrowthfixedParamsEM", # cod is included in package data
          EM_in_dir_vec = EMmodelPath, # EM files
          MS_vec = "EM",       # The management strategy is specified in the EM
          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
          nyrs_vec = nyrs,        # Years to project OM forward
          nyrs_assess_vec = 1, # Years between assessments
          rec_dev_pattern = "vector", # Use user-defined recruitment devs
          rec_dev_pars = t(annChangDevs), # user-provided recruitment devs
          scope = "2", # to use the same recruitment devs across scenarios.
          impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = TRUE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 1234) #Set a fixed integer seed that allows replication

timeEnd <- Sys.time()

print(paste("Time for Nsamp = 20, its = 10, nyrs = 10:", timeEnd - timeStart))

# Test Nsamp = 20 ---------------------------------------------------------

sample_struct$agecomp$Nsamp <- 40

sample_struct$lencomp$Nsamp <- 40

sample_struct_list <- list("5pctAnnChangeNsamp40Model" = sample_struct)

timeStart <- Sys.time()

run_SSMSE(scen_name_vec = "5pctAnnChangeNsamp40Model",# name of the scenario
          out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
          iter_vec = c(10), # run with 5 iterations for now
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = OMmodelPath, # OM files
          EM_name_vec = "constGrowthfixedParamsEM", # cod is included in package data
          EM_in_dir_vec = EMmodelPath, # EM files
          MS_vec = "EM",       # The management strategy is specified in the EM
          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
          nyrs_vec = nyrs,        # Years to project OM forward
          nyrs_assess_vec = 1, # Years between assessments
          rec_dev_pattern = "vector", # Use user-defined recruitment devs
          rec_dev_pars = t(annChangDevs), # user-provided recruitment devs
          scope = "2", # to use the same recruitment devs across scenarios.
          impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = TRUE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 1234) #Set a fixed integer seed that allows replication

timeEnd <- Sys.time()

print(paste("Time for Nsamp = 40, its = 10, nyrs = 10:", timeEnd - timeStart))

# Test Nsamp = 100 ---------------------------------------------------------

sample_struct$agecomp$Nsamp <- 100

sample_struct$lencomp$Nsamp <- 100

sample_struct_list <- list("5pctAnnChangeNsamp100Model" = sample_struct)

timeStart <- Sys.time()

run_SSMSE(scen_name_vec = "5pctAnnChangeNsamp100Model",# name of the scenario
          out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
          iter_vec = c(10), # run with 5 iterations for now
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = OMmodelPath, # OM files
          EM_name_vec = "constGrowthfixedParamsEM", # cod is included in package data
          EM_in_dir_vec = EMmodelPath, # EM files
          MS_vec = "EM",       # The management strategy is specified in the EM
          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
          nyrs_vec = nyrs,        # Years to project OM forward
          nyrs_assess_vec = 1, # Years between assessments
          rec_dev_pattern = "vector", # Use user-defined recruitment devs
          rec_dev_pars = t(annChangDevs), # user-provided recruitment devs
          scope = "2", # to use the same recruitment devs across scenarios.
          impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = TRUE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 1234) #Set a fixed integer seed that allows replication

timeEnd <- Sys.time()

print(paste("Time for Nsamp = 100, its = 10, nyrs = 10:", timeEnd - timeStart))



# Summarize output --------------------------------------------------------

sumry <- SSMSE_summary_all(mseOutputPath, 
                           scenarios = c("5pctAnnChangeNsamp10Model",
                                         "5pctAnnChangeNsamp20Model",
                                         "5pctAnnChangeNsamp40Model"))

recrDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp10Model", termYr = 2028)

recrDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp20Model", termYr = 2028)

recrDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp40Model", termYr = 2028)


compDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp10Model",
              termYr = 2028)


compDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp20Model",
              termYr = 2028)


compDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp40Model",
              termYr = 2028, biomass = FALSE)

compDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
              scenario = "5pctAnnChangeNsamp100Model",
              termYr = 2028, biomass = FALSE)

age1plusDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
                  scenario = "5pctAnnChangeNsamp10Model", termYr = 2028)

age1plusDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
                  scenario = "5pctAnnChangeNsamp20Model", termYr = 2028)

age1plusDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
                  scenario = "5pctAnnChangeNsamp40Model", termYr = 2028)

age1plusDiagPlots(dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMfixedParamsEM",
                  scenario = "5pctAnnChangeNsamp100Model", termYr = 2028)