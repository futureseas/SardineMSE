# De-bugging custom MS for SSMSE

# library(devtools)
# library(tidyverse)
# library(here)
# set_here(path = "C:/Users/Robert W/Documents/FutureSeas")
library(dplyr)
library(r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE
# # 
library(SSMSE) # v0.1.0
#   # RW: This works with old run_SSMSE() formatting, though now it gives updates 
#   #     for each year of the EM applied?
# 
#   # RW: These don't
# remotes::install_github("nmfs-fish-tools/SSMSE")
# library(SSMSE, lib.loc = "C:/Users/rwildermuth/Documents/R/libversions") # v0.2.0
# devtools::load_all(path = "C:/Users/r.wildermuth/Documents/SSMSE")
packageVersion("SSMSE")

# source("R/MS_sar_hcr.R")
# source("R/SourceDiagnosticPlots.R")

# directory for MSE output
mseOutputPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios"

# Operating Model - Research Model ----------------------------------------

# directory for OM SS code
# OMmodelPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start1981/OM/OM_K"
OMmodelPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001"
# RW: need to re-save data.ss_new as data.ss to fix formatting for SS_readdat()

# EM starts in 1981 to test a high data quality scenario
# EMmodelPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start1981/EM_K"
EMmodelPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2005/constantGrowth"
# EM starter.ss file must indicate init values are to be pulled from control.ss file, not ss.par

# Define Observation Model ------------------------------------------------
# Run test of marginal comps OM
datfile <- SS_readdat(file = paste0(OMmodelPath, "/data.ss"), version = "3.30")

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
                   SE = 0.5)

#specify the number of catch fleets
ncdat <- 3

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
sample_struct_list <- list("constGrowthShortOMandEM_RandRec_HCR5" = sample_struct)

# figure out the recruitment deviation input ---------------

# define scenario name
scenName <- "constGrowthShortOMandEM_RandRec_HCR5"
iters <- 3

### Define custom rec devs based on environment

# template <- create_future_om_list(example_type = "custom")
# 
# # recdevPDO <- read.csv("C:/Users/r.wildermuth/Documents/FutureSeas/Recruitment Index/recdevPDO2120.csv")
# # # remove last row
# # recdevPDO <- recdevPDO %>% filter(Year <= yrend - 1)
# 
# recUserDef <- read.csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevSST2070.csv")
# 
# # recUserDef <- read.csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")
# 
# recUserDef <- recUserDef %>% select(year, recDevSST_GFDL) %>%
# # recUserDef <- recUserDef %>% select(Year, recDevPDO) %>%
#                 filter(year <= yrend - 1,
#                        year >= yrsrt - 1) #%>%
#             # mutate(recDevSST_GFDL = case_when(year == 2019 ~ 0,
#             #                                   TRUE ~ recDevSST_GFDL))
# 
# recdevInput <- template[[1]]
# recdevInput$pars <- "rec_devs"
# 
# input <- data.frame(scen = rep(scenName, length.out = iters*nrow(recUserDef)),
#                     iter = rep(1:iters, each = nrow(recUserDef)), # !!RW: must start with empty scenario folder
#                     yr = rep(recUserDef$year, times = iters),
#                     # value = rep(0.15, length.out = iters*nrow(recdevPDO)))
#                     value = rep(recUserDef$recDevSST_GFDL, times = iters))
#                     # value = rep(recUserDef$recDevPDO, times = iters))
# # Add additional error over environment, different among iterations but same across HCRs
# input <- input %>% mutate(addlError = rnorm(nrow(input),0,1.25),
#                           valueNew = value * 0.7 + (0.3 * addlError),
#                           par = "rec_devs")
# recdevInput$input <- input %>% select(par, scen, iter, yr, valueNew) %>%
#                         rename("value" = "valueNew")
# 
# envt_dev_list <- list(recdevInput)

# ### use random recdevs with sd same as to historical
template_mod_change <- create_future_om_list(example_type = "model_change")
rec_dev_specify <- template_mod_change[[1]]
rec_dev_specify$pars <- "rec_devs"
rec_dev_specify$scen <- c("replicate", "all") # note: could change this to c("random", "all") if did not want to replicate the same recdevs sequences across scenarios
rec_dev_specify$input$first_yr_averaging <- datfile$styr
rec_dev_specify$input$last_yr_averaging <- 2019
rec_dev_specify$input$last_yr_orig_val <- 2019
rec_dev_specify$input$first_yr_final_val <- 2020
rec_dev_specify$input$ts_param <- "sd"
rec_dev_specify$input$value <- NA

### Add autocorrelation ###
# new_vals <- data.frame(first_yr_averaging = NA,
#                        last_yr_averaging  = NA, 
#                        last_yr_orig_val   = 2019,
#                        first_yr_final_val = 2020, 
#                        ts_param = "ar_1_phi", 
#                        method = "absolute",
#                        value = 0.5) # 1 for random walk
# rec_dev_specify$input <- rbind(rec_dev_specify$input,
#                                new_vals)

rand_dev_list <- list(rec_dev_specify)

# Run the OM --------------------------------------------------------------

#run_res_path <- file.path("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE", "results")
# dir.create(mseOutputPath)

logFile <- paste0(mseOutputPath, "/SardineMSElog_", Sys.Date(), ".log")

sink(file(logFile), append = TRUE)

startTime <- Sys.time()
ptm <- proc.time()

# Ctemp=read.csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/calcofi_sst_projected.csv")
# Works if not in parallel 

out <- run_SSMSE(scen_name_vec = scenName, # name of the scenario
                 out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
                 iter_vec = c(iters), # run with 5 iterations for now
                 OM_name_vec = NULL, # specify directories instead
                 OM_in_dir_vec = OMmodelPath, # OM files
                 EM_name_vec = "testHCR5", # cod is included in package data
                 EM_in_dir_vec = EMmodelPath, # EM files
                 # MS_vec = "EM",
                 # MS_vec = "no_catch",
                 MS_vec = "MS_sar_hcr5_018",       # The management strategy is specified in the custom function
                 custom_MS_source = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/R/MS_sar_hcr5_018.R", # file location of the MS function
                 use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                 nyrs_vec = nyrs,        # Years to project OM forward
                 nyrs_assess_vec = 1, # Years between assessments
                 future_om_list =  rand_dev_list, # envt_dev_list, #
                 run_parallel = TRUE, # Run iterations in parallel
                 n_cores = 4, # number of cores to use in parallel
                 sample_struct_list = sample_struct_list, # How to sample data for running the EM.
                 seed = 12349) #Set a fixed integer seed that allows replication
endTime <- Sys.time()

procDiff <- proc.time() - ptm

cat("Start time: ", as.character(startTime), "\n")
cat("End time: ", as.character(endTime), "\n")
cat("Processor time difference: \n")
print(procDiff)
cat("\n \n")

out

# close log connection
sink()
# Summarize results -------------------------------------------------------

test1 <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/HCR3_OMK_RandRec/2/margCompsOMandEM_EM_2021")
SS_plots(test1)

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(dir = mseOutputPath, scenarios = scenName, 
                           run_parallel = TRUE)

testFcastOM <- SS_output("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/debugExample/margComps_Fcast0.2/5/margComp_20210921_OM")
SS_plots(testFcastOM)
testFcastEM <- SS_output("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/debugExample/margComps_Fcast0.2/5/margCompsOMfixedSelexEM_EM_2021")
SS_plots(testFcastEM)

testFcastEM <- SS_output("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/debugExample/margComps_Fcast0.2/5/margCompsOMfixedSelexEM_EM_2020",
                         covar = FALSE)
SS_plots(testFcastEM)

testFixed2001OM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/fixedParams2001OM_SD0.25_RandRecHCR2/2/constGrowSelfTest_EM_2021",
                         covar = FALSE)
SS_plots(testFixed2001OM)
