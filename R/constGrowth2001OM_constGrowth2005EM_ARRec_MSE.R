# Code to test a constant growth model starting in 2001 with a random recruitment scenario
# Created: 1/25/2022, Robert Wildermuth
# Details: Uses a constant growth operating model beginning in 2001 and an 
#          estimation model with constant growth and fixed selectivity for time 
#          period 2005-2019.

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
mseOutputPath <- "J:/Desiree/Sardine/SardineScenarios/addlRuns"

# Set Operating and Estimation Model ----------------------------------------

# directory for OM SS code
OMmodelPath <- "../SardineMSE/scenarioModels/start2001"

# EM starts in 1981 to test a high data quality scenario
EMmodelPath <- "../SardineMSE/scenarioModels/start2005"
# EM starter.ss file must indicate init values are to be pulled from control.ss file, not ss.par

# Define Observation Model ------------------------------------------------
# Run test of marginal comps OM
datfile <- SS_readdat(file = paste0(OMmodelPath, "/constGrowthMidSteepNewSelex_OM/data.ss"), version = "3.30")

# create_sample_strct() has trouble IDing SE for survey CPUE
# define an index for the Acoustic-Trawl survey as in Desiree's code
#specify number of years of MSE loop
nyrs <- 50

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
                   SE = 0.25)

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
                      # Use the ~mean values of sample sizes from recent past surveys/fleets
                      Nsamp = c(rep(60,nyrs),rep(50,nyrs),rep(70,nyrs),rep(90,nyrs)))
                      # Nsamp = c(rep(20,nyrs),rep(20,nyrs),rep(20,nyrs),rep(20,nyrs)))

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
                      # Use the ~mean values of sample sizes from recent past surveys/fleets
                      Nsamp = c(rep(80,nyrs),rep(40,nyrs),rep(60,nyrs),rep(80,nyrs)))
                      # Nsamp = c(rep(20,nyrs),rep(20,nyrs),rep(20,nyrs),rep(20,nyrs)))

sample_struct <- list(catch = catch, CPUE = CPUE, lencomp = lencomp, agecomp = agecomp)
sample_struct_list <- list("constGrow2001OM_constGrow2005EM_ARRecHCR0" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR1" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR2" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR3" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR5" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR6" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR7" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR8" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_ARRecHCR9" = sample_struct)

# figure out the recruitment deviation input ---------------

# define scenario name
scenName <- c("constGrow2001OM_constGrow2005EM_ARRecHCR0",
              "constGrow2001OM_constGrow2005EM_ARRecHCR1",
              "constGrow2001OM_constGrow2005EM_ARRecHCR2",
              "constGrow2001OM_constGrow2005EM_ARRecHCR3",
              "constGrow2001OM_constGrow2005EM_ARRecHCR5",
              "constGrow2001OM_constGrow2005EM_ARRecHCR6",
              "constGrow2001OM_constGrow2005EM_ARRecHCR7",
              "constGrow2001OM_constGrow2005EM_ARRecHCR8",
              "constGrow2001OM_constGrow2005EM_ARRecHCR9")
iters <- 400

### use random recdevs with sd same as to historical
template_mod_change <- create_future_om_list(example_type = "model_change")
rec_dev_specify <- template_mod_change[[1]]
rec_dev_specify$pars <- "rec_devs"
rec_dev_specify$scen <- c("replicate", "all") # note: could change this to c("random", "all") if did not want to replicate the same recdevs sequences across scenarios
rec_dev_specify$input$first_yr_averaging <- datfile$styr
rec_dev_specify$input$last_yr_averaging <- 2019
rec_dev_specify$input$last_yr_orig_val <- 2019
rec_dev_specify$input$first_yr_final_val <- 2020
rec_dev_specify$input$ts_param <- "sd"
rec_dev_specify$input$value <- 1.25

new_vals <- data.frame(first_yr_averaging = NA,
                       last_yr_averaging  = NA,
                       last_yr_orig_val   = 2019,
                       first_yr_final_val = 2020,
                       ts_param = "ar_1_phi",
                       method = "absolute",
                       # Use autocorrelation value at 1 lag from recruitmentARanalysis.R
                       value = 0.678) # 1 for random walk
rec_dev_specify$input <- rbind(rec_dev_specify$input,
                               new_vals)

rand_dev_list <- list(rec_dev_specify)

# Run the OM --------------------------------------------------------------

# Custon MS fxn location
MSfxnPath <- "../SardineMSE/R"

# seedNum <- 729 # for first 100
seedNum <- 1104 # for addlRuns

logFile <- paste0(mseOutputPath, "/SardineMSElog_", Sys.Date(), ".log")

sink(file = file(logFile), append = TRUE)

startTime <- Sys.time()
ptm <- proc.time()

out0 <- run_SSMSE(scen_name_vec = scenName[1], # name of the scenario
                 out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                 iter_vec = rep(iters, times = length(scenName[1])), # run with 5 iterations for now
                 OM_name_vec = NULL, # specify directories instead
                 OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                 EM_name_vec = NA, # Can't have number in name for summary diagnostics to work
                 EM_in_dir_vec = NA,
                 MS_vec = "no_catch",
                 # custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr1.R"),
                 use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                 nyrs_vec = nyrs,        # Years to project OM forward
                 nyrs_assess_vec = 1, # Years between assessments
                 future_om_list = rand_dev_list, 
                 run_parallel = TRUE, # Run iterations in parallel
                 sample_struct_list = sample_struct_list[1], # How to sample data for running the EM.
                 seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out0

out1 <- run_SSMSE(scen_name_vec = scenName[2], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[2])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr1", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr1.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[2], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out1

out2 <- run_SSMSE(scen_name_vec = scenName[3], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[3])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr2", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr2.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[3], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out2

out3 <- run_SSMSE(scen_name_vec = scenName[4], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[4])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                      EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                      MS_vec = c("MS_sar_hcr3"),
                      custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr3.R"),
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = rand_dev_list, 
                      run_parallel = TRUE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[4], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out3

out5 <- run_SSMSE(scen_name_vec = scenName[5], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[5])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                      EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                      MS_vec = "MS_sar_hcr5_018", 
                      custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr5_018.R"),
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = rand_dev_list, 
                      run_parallel = TRUE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[5], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out5

out6 <- run_SSMSE(scen_name_vec = scenName[6], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[6])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr6_018", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr6_018.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[6], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out6

out7 <- run_SSMSE(scen_name_vec = scenName[7], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[7])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr7_018", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr7_018.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[7], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out7

out8 <- run_SSMSE(scen_name_vec = scenName[8], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[8])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr8_018", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr8_018.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[8], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out8

out9 <- run_SSMSE(scen_name_vec = scenName[9], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[9])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr9", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr9.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[9], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out9

endTime <- Sys.time()

procDiff <- proc.time() - ptm

cat("Start time: ", as.character(startTime), "\n")
cat("End time: ", as.character(endTime), "\n")
cat("Processor time difference: \n")
print(procDiff)
cat("\n \n")

# close log connection
#sink()
# Summarize results -------------------------------------------------------

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(mseOutputPath,
                           scenarios = scenName, 
                           run_parallel = TRUE)
