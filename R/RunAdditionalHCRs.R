# Code to test a constant growth model starting in 2001 with a random recruitment scenario
# Created: 8/25/2022, Robert Wildermuth
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
mseOutputPath <- "J:/Desiree/Sardine/SardineScenarios"

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
sample_struct_list <- list(#"constGrow2001OM_constGrow2005EM_RegRecHCR2" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_SSTRecHCR9" = sample_struct)

# figure out the recruitment deviation input ---------------

# define scenario name
scenName <- c(#"constGrow2001OM_constGrow2005EM_RegRecHCR2",
              "constGrow2001OM_constGrow2005EM_SSTRecHCR9")
# iters <- 3

### use same recdevs as were used in the HCR0 run
template <- create_future_om_list(example_type = "custom")

recUserDef <- read.csv(file.path(mseOutputPath,
                                 "constGrow2001OM_constGrow2005EM_SSTRecHCR0/results_ts_constGrow2001OM_constGrow2005EM_SSTRecHCR0.csv"))
iters <- max(recUserDef$iteration)

recUserDef <- recUserDef %>% 
                filter(Seas == 1,
                       year <= yrend - 1,
                       year >= yrsrt - 1) %>%
                select(year, rec_dev, iteration) %>%
                rename("yr" = "year",
                       "value" = "rec_dev",
                       "iter" = "iteration") %>% 
                mutate(par = "rec_devs")

recdevInput <- template[[1]]
recdevInput$pars <- "rec_devs"

recdevInput$input <- recUserDef %>% full_join(y = data.frame(scen = scenName), 
                                              by = character()) %>% 
                       select(par, scen, iter, yr, value)

rand_dev_list <- list(recdevInput)

# Run the OM --------------------------------------------------------------

# Custon MS fxn location
MSfxnPath <- "../SardineMSE/R"

seedNum <- 729

logFile <- paste0(mseOutputPath, "/SardineMSElog_", Sys.Date(), ".log")

sink(file = file(logFile), append = TRUE)

startTime <- Sys.time()
ptm <- proc.time()

# rand_dev_list2 <- rand_dev_list
# rand_dev_list2[[1]]$input <- rand_dev_list2[[1]]$input %>% filter(scen %in% scenName[1])
# 
# out2 <- run_SSMSE(scen_name_vec = scenName[1], # name of the scenario
#                   out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
#                   iter_vec = rep(iters, times = length(scenName[1])), # run with 5 iterations for now
#                   OM_name_vec = NULL, # specify directories instead
#                   OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
#                   EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
#                   EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
#                   MS_vec = "MS_sar_hcr2", 
#                   custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr2.R"),
#                   use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
#                   nyrs_vec = nyrs,        # Years to project OM forward
#                   nyrs_assess_vec = 1, # Years between assessments
#                   future_om_list = rand_dev_list2, 
#                   run_parallel = TRUE, # Run iterations in parallel
#                   sample_struct_list = sample_struct_list[1], # How to sample data for running the EM.
#                   seed = seedNum) #Set a fixed integer seed that allows replication
# cat("\n \n")
# out2

rand_dev_list9 <- rand_dev_list
rand_dev_list9[[1]]$input <- rand_dev_list9[[1]]$input %>% filter(scen %in% scenName[1])

out9 <- run_SSMSE(scen_name_vec = scenName[1], # name of the scenario
                  out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                  iter_vec = rep(iters, times = length(scenName[1])), # run with 5 iterations for now
                  OM_name_vec = NULL, # specify directories instead
                  OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                  EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                  EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                  MS_vec = "MS_sar_hcr9", 
                  custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr9.R"),
                  use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                  nyrs_vec = nyrs,        # Years to project OM forward
                  nyrs_assess_vec = 1, # Years between assessments
                  future_om_list = rand_dev_list9, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[1], # How to sample data for running the EM.
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
sink()
# Summarize results -------------------------------------------------------

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(mseOutputPath,
                           scenarios = scenName, 
                           run_parallel = TRUE)
