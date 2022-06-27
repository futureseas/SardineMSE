# Code to apply 3 harvest strategies to a random recruitment scenario
# Created: 11/18/2021, Robert Wildermuth
# Details: Uses a marginal growth operating model and estimation model with 
#          constant growth and fixed selectivity for time period 1981-2019.

# library(here)
# set_here(path = "C:/Users/Robert W/Documents/FutureSeas")
library(dplyr)
library(r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE
# # 
library(SSMSE) 
packageVersion("SSMSE")

# directory for MSE output
# mseOutputPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios"
mseOutputPath <- "J:/Desiree/Sardine/SardineScenarios"

# Operating Model - Research Model ----------------------------------------

# directory for OM SS code
# OMmodelPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/OM/OM_K"
OMmodelPath <- "J:/Desiree/Sardine/SardineMSE/OM/OM_K"


# Define Observation Model ------------------------------------------------
# Run test of marginal comps OM
datfile <- SS_readdat(file = paste0(OMmodelPath, "/dat.ss"), version = "3.30")

# create_sample_strct() has trouble IDing SE for survey CPUE
# define an index for the Acoustic-Trawl survey as in Desiree's code
#specify number of years of MSE loop
nyrs <- 20

#sample_struct <- create_sample_struct(dat = datfile, nyrs = nyrs)
#traceback()

#specify the start year of data inputs
yrsrt <- datfile$endyr +1

#specify the end year of data inputs
yrend <- datfile$endyr + nyrs

#sample_struct$CPUE = sample_struct$CPUE[1:nyrs,]
CPUE <- data.frame(Yr= yrsrt:yrend,
                   Seas= 1,
                   FltSvy = 4,
                   SE = 0.5)

#specify the number of catch fleets
ncdat <- 3

catch <- data.frame(Yr = rep(c(yrsrt:yrend),ncdat), 
                    Seas = c(rep(1,nyrs),rep(2,nyrs)),
                    FltSvy = c(rep(1,nyrs*2),rep(2,nyrs*2),rep(3,nyrs*2)),
                    SE = 0.05)

#for length comps use AT summer survey (fleet #4 - Nsamp15), mexcal s1 (fleet #1 Nsamp 20), mexcal s2 (fleet #2-Nsamp 40),
#pnw (fleet #3 Nsamp 30 s1)
#the .dat file specifies month...but here it is labeled as season?
#specify the number of lengthcomp surveys
nldat <- 4
lencomp <- data.frame(Yr = rep(c(yrsrt:yrend),nldat), 
                      Seas = c(rep(1,nyrs),rep(4,nyrs),rep(10,nyrs),rep(4,nyrs)),
                      FltSvy = c(rep(4,nyrs),rep(1,nyrs),rep(2,nyrs),rep(3,nyrs)),
                      Sex = rep(0,nyrs*nldat),
                      Part = rep(0,nyrs*nldat),
                      Nsamp = c(rep(20,nyrs),rep(20,nyrs),rep(20,nyrs),rep(20,nyrs)))

#for age comps same surveys as as lcomps
nadat <- 4
agecomp <- data.frame(Yr = rep(c(yrsrt:yrend),nadat), 
                      Seas = c(rep(1,nyrs),rep(4,nyrs),rep(10,nyrs),rep(4,nyrs)),
                      FltSvy = c(rep(4,nyrs),rep(1,nyrs),rep(2,nyrs),rep(3,nyrs)),
                      Sex = rep(0,nyrs*nadat),
                      Part = rep(0,nyrs*nadat),
                      Ageerr = c(rep(4,nyrs),rep(4,nyrs),rep(4,nyrs),rep(4,nyrs)),
                      Lbin_lo = c(rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs)),
                      Lbin_hi = c(rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs),rep(-1,nyrs)),
                      Nsamp = c(rep(20,nyrs),rep(20,nyrs),rep(20,nyrs),rep(20,nyrs)))

sample_struct <- list(catch = catch, CPUE = CPUE, lencomp = lencomp, agecomp = agecomp)
sample_struct_list <- list("margCompsOMfixedSelexEM_RandRecst2005" = sample_struct)
# sample_struct_list <- list(#"margCompsOMfixedSelexEM_RandRecHCR0" = sample_struct,
#                            #"margCompsOMfixedSelexEM_RandRecHCR1" = sample_struct,
#                            "margCompsOMfixedSelexEM_RandRecHCR4" = sample_struct,
#                            #"margCompsOMfixedSelexEM_RandRecHCR5" = sample_struct,
#                            "margCompsOMfixedSelexEM_RandRecHCR6" = sample_struct)
# figure out the recruitment deviation input ---------------

# define scenario name
scenName <- c("margCompsOMfixedSelexEM_RandRecst2005")
# scenName <- c(#"margCompsOMfixedSelexEM_RandRecHCR0",
#               #"margCompsOMfixedSelexEM_RandRecHCR1",
#               "margCompsOMfixedSelexEM_RandRecHCR4",
#               #"margCompsOMfixedSelexEM_RandRecHCR5",
#               "margCompsOMfixedSelexEM_RandRecHCR6")
iters <- 50

### use random recdevs with sd same as to historical
template_mod_change <- create_future_om_list(example_type = "model_change")
rec_dev_specify <- template_mod_change[[1]]
rec_dev_specify$pars <- "rec_devs"
rec_dev_specify$scen <- c("replicate", "all") # note: could change this to c("random", "all") if did not want to replicate the same recdevs sequences across scenarios
rec_dev_specify$input$first_yr_averaging <- 1981
rec_dev_specify$input$last_yr_averaging <- 2019
rec_dev_specify$input$last_yr_orig_val <- 2019
rec_dev_specify$input$first_yr_final_val <- 2020
rec_dev_specify$input$ts_param <- "sd"
rec_dev_specify$input$value <- NA

rand_dev_list <- list(rec_dev_specify)

# Run the OM --------------------------------------------------------------

# EM starts in 1981 to test a high data quality scenario
# EMmodelPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/EM"
EMmodelPath <- "J:/Desiree/Sardine/SardineMSE/EM"
# EM starter.ss file must indicate init values are to be pulled from control.ss file, not ss.par

startTime <- Sys.time()
out <- run_SSMSE(scen_name_vec = scenName, # name of the scenario
                 out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                 iter_vec = rep(iters, times = length(scenName)), # run with 5 iterations for now
                 OM_name_vec = NULL, # specify directories instead
                 OM_in_dir_vec = OMmodelPath, #rep(OMmodelPath, times = length(scenName)), # OM files
                 EM_name_vec = "margCompsOMfixedSelexEM", #c(NA, rep("margCompsOMfixedSelexEM", times = length(scenName)-1)), # cod is included in package data
                 EM_in_dir_vec = file.path(EMmodelPath, "EM_st2005"),
                   # c(#NA,
                   #                 #file.path(EMmodelPath, "EM_st2005"),
                   #                 file.path(EMmodelPath, "EM_HCR4"),
                   #                 #file.path(EMmodelPath, "EM_HCR5"),
                   #                 file.path(EMmodelPath, "EM_HCR6")), # EM files
                 MS_vec = "EM", #c("EM", "EM"),
                   # c("no_catch", "MS_sar_hcr",
                   #   rep("EM", times = 3)),
                 # custom_MS_source = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/R/MS_sar_hcr.R", # file location of the MS function
                 # custom_MS_source = "J:/Desiree/Sardine/SardineMSE/R/MS_sar_hcr.R",
                 use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                 nyrs_vec = nyrs,        # Years to project OM forward
                 nyrs_assess_vec = 1, # Years between assessments
                 future_om_list = rand_dev_list, 
                 run_parallel = TRUE, # Run iterations in parallel
                 # n_cores = 20,
                 sample_struct_list = sample_struct_list, # How to sample data for running the EM.
                 seed = 1234) #Set a fixed integer seed that allows replication
 endTime <- Sys.time()
# Summarize results -------------------------------------------------------

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(mseOutputPath,
                           scenarios = scenName, # won't work for the no catch scenario
                           run_parallel = TRUE)
