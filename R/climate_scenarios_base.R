#created by D. Tommasi starting on 4/23/2026
#Code to run Wildermuth's et al.2024 MSE under no catch for 100 iterations with autocorrelated recruitment process error 
#This is the base OM simulation
#the operating model used is the constant growth model starting in 2001 used by Wildermuth's et al.2024

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

#maintain the same observation error across uncertainty scenarios
sample_struct_list <- list("No_Climate" = sample_struct,
                           "M_climate" = sample_struct,
                           "G_climate" = sample_struct,
                           "Q_climate" = sample_struct,
                           "DDepM" = sample_struct)

# define scenario name
scenName <- c("No_Climate",
              "M_climate",
              "G_climate",
              "Q_climate",
              "DDepM")

#set the number of iterations
iters <- 100

################Autocorrelated recruitment###################################
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

# Custom MS fxn location
MSfxnPath <- paste0(wdir,"/R")

seedNum <- 1104
startTime <- Sys.time()
ptm <- proc.time()

outNoclim <- run_SSMSE(scen_name_vec = scenName[1], # name of the scenario
                      out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                      iter_vec = rep(iters, times = length(scenName[1])), # run with 5 iterations for now
                      OM_name_vec = NULL, # specify directories instead
                      OM_in_dir_vec = file.path(OMmodelPath, "constGrowthMidSteepNewSelex_OM"), #rep(OMmodelPath, times = length(scenName)), # OM files
                      MS_vec = "no_catch", 
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      future_om_list = rand_dev_list, 
                      run_parallel = FALSE, # Run iterations in parallel
                      sample_struct_list = sample_struct_list[1], # How to sample data for running the EM.
                      seed = seedNum) #Set a fixed integer seed that allows replication
endTime <- Sys.time()
procDiff <- proc.time() - ptm