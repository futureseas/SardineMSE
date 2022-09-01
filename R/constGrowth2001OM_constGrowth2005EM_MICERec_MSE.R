# Code to test a constant growth model starting in 2001 with recruitment from the sardine MICE model
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
sample_struct_list <- list("constGrow2001OM_constGrow2005EM_MICERecHCR0" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR1" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR2" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR3" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR5" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR6" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR7" = sample_struct,
                           "constGrow2001OM_constGrow2005EM_MICERecHCR8" = sample_struct)

# figure out the recruitment deviation input ---------------

# define scenario name
scenName <- c("constGrow2001OM_constGrow2005EM_MICERecHCR0",
              "constGrow2001OM_constGrow2005EM_MICERecHCR1",
              "constGrow2001OM_constGrow2005EM_MICERecHCR2",
              "constGrow2001OM_constGrow2005EM_MICERecHCR3",
              "constGrow2001OM_constGrow2005EM_MICERecHCR5",
              "constGrow2001OM_constGrow2005EM_MICERecHCR6",
              "constGrow2001OM_constGrow2005EM_MICERecHCR7",
              "constGrow2001OM_constGrow2005EM_MICERecHCR8")
iters <- 100

### Define custom rec devs based on environment

template <- create_future_om_list(example_type = "custom")

recUserDef <- read.csv("../SardineMSE/dat/recdevMICE2100.csv")

recUserDef <- recUserDef %>% 
                filter(Year <= yrend - 1,
                       Year >= yrsrt - 1,
                       GCM == "gcmMEAN") %>%
                select(Year, ensembleRecDevs) 

recdevInput <- template[[1]]
recdevInput$pars <- "rec_devs"

input <- data.frame(iter = rep(1:iters, each = nrow(recUserDef)), # !!RW: must start with empty scenario folder
                    yr = rep(recUserDef$Year, times = iters),
                    value = rep(recUserDef$ensembleRecDevs, times = iters))
# Add additional error over environment, different among iterations but same across HCRs
input <- input %>% mutate(addlError = rnorm(nrow(input),0, 1.25),
                          valueNew = value * 0.7 + (0.3 * addlError),
                          par = "rec_devs",
                          devSD = sd(valueNew))
# do scale correction
input <- input %>% mutate(valueNew = valueNew * (1.25/devSD))
input <- input %>% full_join(y = data.frame(scen = scenName), by = character()) %>% 
  arrange(scen, iter, yr)
recdevInput$input <- input %>% select(par, scen, iter, yr, valueNew) %>%
  rename("value" = "valueNew")

envt_dev_list <- list(recdevInput)

# Run the OM --------------------------------------------------------------

# Custom MS fxn location
# MSfxnPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/R"
MSfxnPath <- "J:/Desiree/Sardine/SardineMSE/R"
seedNum <- 729
# logFile <- paste0(mseOutputPath, "/SardineMSElog_", Sys.Date(), ".log")
# 
# sink(file = file(logFile), append = TRUE)

startTime <- Sys.time()
ptm <- proc.time()


envt_dev_list0and1 <- envt_dev_list
envt_dev_list0and1[[1]]$input <- envt_dev_list0and1[[1]]$input %>% filter(scen %in% scenName[1:2])

out0and1 <- run_SSMSE(scen_name_vec = scenName[1:2], # name of the scenario
                 out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                 iter_vec = rep(iters, times = length(scenName[1:2])), # run with 5 iterations for now
                 OM_name_vec = NULL, # specify directories instead
                 OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                 EM_name_vec = c(NA, "constGrowBothShort"), # Can't have number in name for summary diagnostics to work
                 EM_in_dir_vec = c(NA, 
                                   file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM")),
                 MS_vec = c("no_catch", 
                            "MS_sar_hcr1"),
                 custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr1.R"),
                 use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                 nyrs_vec = nyrs,        # Years to project OM forward
                 nyrs_assess_vec = 1, # Years between assessments
                 future_om_list = envt_dev_list0and1, 
                 run_parallel = TRUE, # Run iterations in parallel
                 sample_struct_list = sample_struct_list[1:2], # How to sample data for running the EM.
                 seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out0and1

envt_dev_list2 <- envt_dev_list
envt_dev_list2[[1]]$input <- envt_dev_list2[[1]]$input %>% filter(scen %in% scenName[3])

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
                  future_om_list = envt_dev_list2, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[3], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out2

envt_dev_list3 <- envt_dev_list
envt_dev_list3[[1]]$input <- envt_dev_list3[[1]]$input %>% filter(scen %in% scenName[4])

out3 <- run_SSMSE(scen_name_vec = scenName[4], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[4])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      EM_name_vec = "constGrowBothShort", # Can't have number in name for summary diagnostics to work
                      EM_in_dir_vec = file.path(EMmodelPath, "constGrowthMidSteepNewSelex_EM"),
                      MS_vec = "MS_sar_hcr3",
                      custom_MS_source = file.path(MSfxnPath, "MS_sar_hcr3.R"),
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = envt_dev_list3, 
                      run_parallel = TRUE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[4], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out3

envt_dev_list5 <- envt_dev_list
envt_dev_list5[[1]]$input <- envt_dev_list5[[1]]$input %>% filter(scen %in% scenName[5])

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
                      future_om_list = envt_dev_list5, 
                      run_parallel = TRUE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[5], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out5

envt_dev_list6 <- envt_dev_list
envt_dev_list6[[1]]$input <- envt_dev_list6[[1]]$input %>% filter(scen %in% scenName[6])

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
                  future_om_list = envt_dev_list6, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[6], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out6

envt_dev_list7 <- envt_dev_list
envt_dev_list7[[1]]$input <- envt_dev_list7[[1]]$input %>% filter(scen %in% scenName[7])

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
                  future_om_list = envt_dev_list7, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[7], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out7

envt_dev_list8 <- envt_dev_list
envt_dev_list8[[1]]$input <- envt_dev_list8[[1]]$input %>% filter(scen %in% scenName[8])

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
                  future_om_list = envt_dev_list8, 
                  run_parallel = TRUE, # Run iterations in parallel
                  sample_struct_list = sample_struct_list[8], # How to sample data for running the EM.
                  seed = seedNum) #Set a fixed integer seed that allows replication
cat("\n \n")
out8


endTime <- Sys.time()

procDiff <- proc.time() - ptm

cat("Start time: ", as.character(startTime), "\n")
cat("End time: ", as.character(endTime), "\n")
cat("Processor time difference: \n")
print(procDiff)
cat("\n \n")

# close log connection
# sink()
# Summarize results -------------------------------------------------------

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(mseOutputPath,
                           scenarios = scenName, 
                           run_parallel = TRUE)
