# Function to extract output for all OM and EM SSMSE iterations
# Created: 11/30/2021; Robert Wildermuth

# modified function to get mean Francis composition using weighting method in TA1.8
# source("~/SardineMSE/R/SSMethod.TA1.8mod.R")
library(r4ss)
library(tidyverse)
library(doParallel)

GetSumryOutput <- function(dirSSMSE, # SSMSE directory (character)
                           scenarios, # scenario name (character)
                           comps = FALSE, # return comps output? (logical) Default is FALSE
                           simData = FALSE # return simulated EM data? (logical) Default is FALSE
){
  
  # Pull the SSMSE summary info ---------------------------------------------
  dqSmryAll <- sclSmryAll <- tsSmryAll <- NULL
  
  for(scn in 1:length(scenarios)){
    # read in SSMSE results summary derived quantities, scalars, and time series
    dqSumry <- read_csv(file.path(dirSSMSE, scenarios[scn], 
                                  paste0("results_dq_", scenarios[scn], ".csv")))
    dqSumry <- dqSumry[, c("Value.Recr", "Value.SSB", "year", "model_run", "iteration", "scenario")]
    sclSumry <- read.csv(file.path(dirSSMSE, scenarios[scn], 
                                   paste0("results_scalar_", scenarios[scn], ".csv")))
    if(!"F_MSY" %in% names(sclSumry)){ # no catch scenarios don't have F_MSY
      sclSumry$F_MSY <- NA
      sclSumry$SSB_Unfished <- NA
    }
    sclSumry <- sclSumry[, c("F_MSY", "SmryBio_Unfished", "SSB_Unfished",
                             "max_grad", "model_run", "iteration", "scenario")]
    tsSumry <- read.csv(file.path(dirSSMSE, scenarios[scn], 
                                  paste0("results_ts_", scenarios[scn], ".csv")))
    tsSumry <- tsSumry[, c("Bio_smry", "retainB_1", "retainB_2", "retainB_3", "rec_dev",
                           "year", "Seas", "model_run", "iteration", "scenario")]
    
    dqSmryAll <- bind_rows(dqSmryAll, dqSumry)
    sclSmryAll <- bind_rows(sclSmryAll, sclSumry)
    tsSmryAll <- bind_rows(tsSmryAll, tsSumry)
  } # end 'scn' for-loop
  
  outList <- list("dqSmry" = dqSmryAll, "sclSmry" = sclSmryAll, "tsSmry" = tsSmryAll)
  
  # Use parallelization to pull in composition and EM data ------------------
  # Adapted from code from Peter Kuriyama
  
  # set up the directories
  # get the iterations
  resultsDirs <- NULL
  for(scn in 1:length(scenarios)){
    iters <- list.dirs(file.path(dirSSMSE, scenarios[scn]), recursive = FALSE, full.names = FALSE)
  
    # get the model directory names
    runNames <- list.dirs(file.path(dirSSMSE, scenarios[scn], iters[1]),
                           recursive = FALSE,
                           full.names = FALSE)
  
    # Need to remove EM folders for HCRs that don't run the EM
    if(grepl("HCR9", scenarios[scn], fixed = TRUE)){
      runNames <- grep("_OM", runNames, value = TRUE, fixed = TRUE)
    }
    
    #The results directories to read in
    scnResultsDirs <- expand_grid(scenarios[scn], iters, runNames) %>% 
                    mutate(scen = file.path(dirSSMSE, `scenarios[scn]`, iters, runNames)) %>%
                    pull(scen)
    
    resultsDirs <- c(resultsDirs, scnResultsDirs)
  }
  
  if(comps){
    # extract wanted tables per directory and add data origin
    # start_time <- Sys.time()
    ncores <- detectCores() - 2 #Leave some cores open for background stuff
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    
    resultsList <- foreach::foreach(ii = 1:length(resultsDirs),
                                    
                                    .packages = c("tidyverse", 'r4ss')) %dopar% {
                                      outList <- SS_output(resultsDirs[ii], 
                                                covar = FALSE, printstats = FALSE,
                                                verbose = FALSE)
                                      outList %>% magrittr::extract(c("len_comp_fit_table", 
                                                                      "age_comp_fit_table")) %>%
                                        map2(.y = resultsDirs[ii], 
                                             .f = function(x, y){x['resDir'] <- y;x})
                                    }
    stopCluster(cl)
    # run_time <- Sys.time() - start_time; run_time #To see how long it takes
    
    # summarize into single table for export
    smryMeanLen <- resultsList %>% map_dfr(magrittr::extract2, "len_comp_fit_table") %>%
                      select(Fleet, Fleet_Name, Yr, Seas, All_obs_mean, All_exp_mean, resDir)
    smryMeanAge <- resultsList %>% map_dfr(magrittr::extract2, "age_comp_fit_table") %>%
                      select(Fleet, Fleet_Name, Yr, Seas, All_obs_mean, All_exp_mean, resDir)
    
    outList$lenComp <- smryMeanLen
    outList$ageComp <- smryMeanAge
  } # end 'comps' if-statement
  
  if(simData){
    # repeat for the data file
    ncores <- detectCores() - 2 #Leave some cores open for background stuff
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    
    dataList <- foreach::foreach(ii = 1:length(resultsDirs),
                                    
                                    .packages = c("tidyverse", 'r4ss')) %dopar% {
                                      outList <- SS_readdat(file = file.path(resultsDirs[ii], "data.ss_new"),
                                                            version = "3.30", verbose = FALSE)
                                      outList %>% magrittr::extract(c("CPUE", 
                                                                      "catch")) %>%
                                        map2(.y = resultsDirs[ii], 
                                             .f = function(x, y){x['resDir'] <- y;x})
                                    }
    stopCluster(cl)
  
    # summarize into single table for export
    smryObsCPUE <- dataList %>% map_dfr(magrittr::extract2, "CPUE") 
    smryObsCatch <- dataList %>% map_dfr(magrittr::extract2, "catch") 
    
    outList$obsCPUE <- smryObsCPUE
    outList$obsCatch <- smryObsCatch
  
  } # end 'simData' if-statement
  
  return(outList)
}
