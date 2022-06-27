# Function to extract output for all OM and EM SSMSE iterations and plot time 
# series of variables of interest
# Created: 5/12/2022; Robert Wildermuth

# modified function to get mean Francis composition using weighting method in TA1.8
# source("~/SardineMSE/R/SSMethod.TA1.8mod.R")
library(r4ss)
library(tidyverse)

PlotEMAnnualEsts <- function(dirSSMSE, # SSMSE directory (character)
                           scenarios, # scenario name (character)
                           varCol, # character string of column names for variables to plot
                           convgLevel = 0.01 # numeric convergence level
){
  
  # Pull the SSMSE summary info ---------------------------------------------
  dqSmryAll <- sclSmryAll <- tsSmryAll <- NULL
  
  for(scn in 1:length(scenarios)){
    # read in SSMSE results summary derived quantities, scalars, and time series
    dqSumry <- read_csv(file.path(dirSSMSE, scenarios[scn], 
                                  paste0("results_dq_", scenarios[scn], ".csv")))
    #dqSumry <- dqSumry[, c("Value.Recr", "Value.SSB", "year", "model_run", "iteration", "scenario")]
    sclSumry <- read.csv(file.path(dirSSMSE, scenarios[scn], 
                                   paste0("results_scalar_", scenarios[scn], ".csv")))
    if(!"F_MSY" %in% names(sclSumry)){ # no catch scenarios don't have F_MSY
      sclSumry$F_MSY <- NA
      sclSumry$SSB_Unfished <- NA
    }
    #sclSumry <- sclSumry[, c("F_MSY", "SmryBio_Unfished", "SSB_Unfished",
    #                         "max_grad", "model_run", "iteration", "scenario")]
    tsSumry <- read.csv(file.path(dirSSMSE, scenarios[scn], 
                                  paste0("results_ts_", scenarios[scn], ".csv")))
    #tsSumry <- tsSumry[, c("Bio_smry", "retainB_1", "retainB_2", "retainB_3", "rec_dev",
    #                       "year", "Seas", "model_run", "iteration", "scenario")]
    
    dqSmryAll <- bind_rows(dqSmryAll, dqSumry)
    sclSmryAll <- bind_rows(sclSmryAll, sclSumry)
    tsSmryAll <- bind_rows(tsSmryAll, tsSumry)
  } # end 'scn' for-loop
  
  omName <- unique(grep("_OM", tsSmryAll$model_run, fixed = TRUE, value = TRUE))
  
  # ID convergence status
  convgID <- sclSmryAll %>% select(max_grad, model_run, iteration, scenario, any_of(varCol)) %>%
                mutate(plotGroup = case_when(model_run %in% omName ~ "OM",
                                             max_grad > convgLevel ~ "non-convrg",
                                             max_grad < convgLevel ~ "convrg"),
                       year = as.numeric(sub(pattern = ".*M_","", model_run)))
                
  dqSmryAll <- dqSmryAll %>% select(model_run, iteration, scenario, 
                                    Value.Recr, year, any_of(varCol)) %>%
                  mutate(logRecr = log(Value.Recr)) %>%
                  left_join(y = convgID[, c("model_run", "iteration", "scenario", "plotGroup")], 
                            by = c("iteration", "model_run", "scenario")) %>%
                  select(!Value.Recr) %>%
                  pivot_longer(cols = c(logRecr, any_of(varCol)), 
                               names_to = "variable", values_to = "vals") 
                
  tsSmryAll <- tsSmryAll %>% filter(Seas == 1) %>%
                  select(model_run, iteration, scenario, 
                         Bio_smry, year, any_of(varCol)) %>%
                  mutate(logAge1Plus = log(Bio_smry)) %>%
                  left_join(y = convgID[, c("model_run", "iteration", "scenario", "plotGroup")], 
                            by = c("iteration", "model_run", "scenario")) %>%
                  select(!Bio_smry) %>%
                  pivot_longer(cols = c(logAge1Plus, any_of(varCol)), 
                               names_to = "variable", values_to = "vals")            
  
  tsVals <- convgID %>% pivot_longer(cols = any_of(varCol), 
                                     names_to = "variable", values_to = "vals") %>%
              select(!max_grad) %>%
              mutate(model_run = sub("[[:digit:]]+", "", model_run))
  
  tsVals <- rbind(tsVals, dqSmryAll, tsSmryAll)
  # add info about HCR and scenario
  tsVals <- tsVals %>% mutate(HCR = sub(pattern = ".*Rec","", scenario),
                              recScen = sub(pattern = "HCR.*","", scenario)) %>%
              mutate(recScen = sub(pattern = ".*OM_","", recScen))
  
  # make the plots
  scen <- unique(tsVals$scenario)
  
  for(i in 1:length(scen)){
    print(tsVals %>% filter(scenario == scen[i]) %>%
            ggplot(aes(x = year, y = vals)) +
            geom_vline(xintercept = 2019, color = "gray") +
            geom_line(aes(linetype = model_run, color = plotGroup))+
            scale_color_manual(values = c("black", "blue", "#D65F00")) +
            scale_linetype_manual(values = rep("solid", 51)) +
            guides(linetype = "none") +
            facet_grid(rows = vars(variable), cols = vars(iteration), scales = "free") +
            theme_classic() + theme(legend.position="none") +
            labs(title = scen[i]))
  }
  
}
