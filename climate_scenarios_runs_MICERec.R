#created by D. Tommasi starting on 4/23/2026
#Code to run Wildermuth's et al.2024 MSE under no catch for 100 iterations with DynaMICE recruitment plus process error 
#runs each of the climate infomred OM scenarios informed by ecological model output
#the base operating model used is the constant growth model starting in 2001 used by Wildermuth's et al.2024
#this was modified to use DynaMICe recruitment (R_climate scenario) as well as

#M modified to use the one informed by Atlantis - M-climate
#Linf, K, and L1 modified to use the one informed by IBM under GFDL - G_climate2
#Linf, K, and L1 modified to use the one informed by IBM under Hadley - G_climate
#M from Atlantis and growth from Hadley - MG_climate
#also a climate informed catchability scenario (Q_climate) could be developed from the SDMs if one was to use an EM, not done

#scenarios:
sample_struct_list <- list("MG_Climate" = sample_struct,
                           "R_climate" = sample_struct,
                           "M_climate" = sample_struct,
                           "G_climate" = sample_struct,
                           "Q_climate" = sample_struct,
                           "G_climate2" = sample_struct)

#instead of loading SSMSE package load local modified package available at https://github.com/detommas/SSMSEsar
#modifications were done to use latest version of r4ss no matter what the colum headers are
setwd("C:/Users/desiree.tommasi/Documents/SSMSEsar-main/R")
#source all the SSMSE functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)


library(dplyr)
library(r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE

#set working directory
setwd("C:/Users/desiree.tommasi/Documents/CAFA/Synthesis/SardineMSE-main")
wdir <- getwd()

# directory for MSE output
mseOutputPath <- "C:/Users/desiree.tommasi/Documents/CAFA/Synthesis/OM_outputs"

# Set Operating Model ----------------------------------------
#Note Estimation model not required as no catch scenario

# directory for OM SS code
OMmodelPath <- paste0(wdir,"/scenarioModels/start2001")

# Define Observation Model ------------------------------------------------
# Run test of marginal comps OM
datfile <- SS_readdat(file = paste0(OMmodelPath, "/constGrowthMidSteepNewSelex_OM/data.ss"), version = "3.30")

# define an index for the Acoustic-Trawl survey as in Desiree's code
#specify number of years of MSE loop
nyrs <- 50

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

sample_struct <- list(catch = catch, CPUE = CPUE, lencomp = lencomp, agecomp = agecomp)

#maintain the same observation error across uncertainty scenarios
sample_struct_list <- list("MG_Climate" = sample_struct,
                           "R_climate" = sample_struct,
                           "M_climate" = sample_struct,
                           "G_climate" = sample_struct,
                           "Q_climate" = sample_struct,
                           "G_climate2" = sample_struct)

# define scenario name
scenName <- c("MG_Climate",
              "R_climate",
              "M_climate",
              "G_climate",
              "Q_climate",
              "G_climate2")

#set the number of iterations
iters <- 100

################DynaMICE Recruitment SCENARIO###################################
#Define custom rec devs based on MICE output
template <- create_future_om_list(example_type = "custom")
recUserDef <- read.csv(paste0(wdir,"/dat/recdevMICE2100.csv"))

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

#save recdevs for future plotting
write.csv(envt_dev_list[[1]]$input, "C:/Users/desiree.tommasi/Documents/CAFA/Synthesis/MSEscenarios/dat/FutureMICERecdevs.csv")
#save historical recdevs from OM
Omout <- SS_output(paste0(OMmodelPath, "/constGrowthMidSteepNewSelex_OM"))
write.csv(Omout$recruit, "C:/Users/desiree.tommasi/Documents/CAFA/Synthesis/MSEscenarios/dat/HistOMRecdevs.csv")


# Run the OM --------------------------------------------------------------

# Custom MS fxn location
MSfxnPath <- paste0(wdir,"/R")

seedNum <- 1104
startTime <- Sys.time()
ptm <- proc.time()

envt_dev_list2 <- envt_dev_list
envt_dev_list2[[1]]$input <- envt_dev_list2[[1]]$input %>% filter(scen %in% scenName[2])

outRclim <- run_SSMSE(scen_name_vec = scenName[2], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[2])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      MS_vec = "no_catch", 
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = envt_dev_list2, 
                      run_parallel = FALSE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[2], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
endTime <- Sys.time()
procDiff <- proc.time() - ptm

################M SCENARIO###################################
#Still define custom rec devs based on MICE output since Atlantis was run with MICE Recs
#the modified OMs are on https://github.com/futureseas/SardineMSE/tree/main/scenarioModels/start2001 

envt_dev_list2 <- envt_dev_list
envt_dev_list2[[1]]$input <- envt_dev_list2[[1]]$input %>% filter(scen %in% scenName[3])

#but refers to another OM folder with M changed from 0.585 to 0.635
seedNum <- 1104
startTime <- Sys.time()
ptm <- proc.time()

outMclim <- run_SSMSE(scen_name_vec = scenName[3], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[3])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "Mclimate_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      MS_vec = "no_catch", 
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = envt_dev_list2, 
                      run_parallel = FALSE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[3], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
endTime <- Sys.time()
procDiff <- proc.time() - ptm

################G SCENARIO Hadley###################################
#Still define custom rec devs based on MICE output since Atlantis was run with MICE Recs

envt_dev_list2 <- envt_dev_list
envt_dev_list2[[1]]$input <- envt_dev_list2[[1]]$input %>% filter(scen %in% scenName[4])

#but refers to another OM folder with M changed from 0.585 to 0.635
seedNum <- 1104
startTime <- Sys.time()
ptm <- proc.time()

outGclim <- run_SSMSE(scen_name_vec = scenName[4], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[4])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "Ghadclimate_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      MS_vec = "no_catch", 
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = envt_dev_list2, 
                      run_parallel = FALSE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[4], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
endTime <- Sys.time()
procDiff <- proc.time() - ptm

################G SCENARIO GFDL###################################
#Still define custom rec devs based on MICE output since Atlantis was run with MICE Recs

envt_dev_list2 <- envt_dev_list
envt_dev_list2[[1]]$input <- envt_dev_list2[[1]]$input %>% filter(scen %in% scenName[6])

#but refers to another OM folder with M changed from 0.585 to 0.635
seedNum <- 1104
startTime <- Sys.time()
ptm <- proc.time()

outG2clim <- run_SSMSE(scen_name_vec = scenName[6], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[6])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "Ggfdclimate_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      MS_vec = "no_catch", 
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = envt_dev_list2, 
                      run_parallel = FALSE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[6], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
endTime <- Sys.time()
procDiff <- proc.time() - ptm

################M and G SCENARIO Hadley and M###################################
#Still define custom rec devs based on MICE output since Atlantis was run with MICE Recs

envt_dev_list2 <- envt_dev_list
envt_dev_list2[[1]]$input <- envt_dev_list2[[1]]$input %>% filter(scen %in% scenName[1])

#but refers to another OM folder with M changed from 0.585 to 0.635 and von bertalannfy parameters changed to 
#Linf = 23.4844373 
#L1 = 12.8204896 
#K =  0.2943888  
seedNum <- 1104
startTime <- Sys.time()
ptm <- proc.time()

outMGclim <- run_SSMSE(scen_name_vec = scenName[1], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[1])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "MGhadclimate_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      MS_vec = "no_catch", 
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = envt_dev_list2, 
                      run_parallel = FALSE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[1], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
endTime <- Sys.time()
procDiff <- proc.time() - ptm
