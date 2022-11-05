# Calculate performance metrics from the operating model output
# Created: 10/26/2021, Robert Wildermuth

CalcPerformance <- function(runSmryOutput){
  termYr <- max(runSmryOutput$tsSmry$year)
  
  omName <- grep("_OM", runSmryOutput$tsSmry$model_run, 
                 fixed = TRUE, value = TRUE)[1]
  
  # Record convergence metric
  smryConvrg <- runSmryOutput$sclSmry %>% select(max_grad, model_run, iteration, scenario) %>%
                  mutate(emYear = as.numeric(regmatches(model_run,
                                                        gregexpr("[[:digit:]]+", 
                                                                 model_run)))) %>%
                  filter(max_grad > 0.01) %>%
                  dplyr::group_by(iteration, scenario) %>%
                  summarize(nonconvg = length(max_grad)) %>%
                  mutate(nYrs = termYr - 2019,
                         frqNonConvg = nonconvg/nYrs)
  
  # Fishery closure cutoff
  cutoff <- 150000
  # collapse threshold
  collapseThresh <- 50000
  # biomass at which maximum catch of 200000 reached when Fmsy = 0.05
  maxcat <- 4150000
  # bonanza biomass defined in Hurtado-Ferro & Punt 2014 performance metrics
  bonanzaBio <- 400000
  
  # biomass metrics
  tsBio <- runSmryOutput$tsSmry %>% filter(model_run == omName) %>%
              filter(Seas == 1,
                     year > 2019) %>%
              mutate(closure = Bio_smry < cutoff,
                     collapse = Bio_smry < collapseThresh,
                     bonanza = Bio_smry > bonanzaBio) %>%
              group_by(model_run, iteration, scenario) %>%
              mutate(closureLength = sequence(rle(closure)$lengths),
                     collapseLength = sequence(rle(collapse)$lengths),
                     bonanzaLength = sequence(rle(bonanza)$lengths),
                     collapseSever = case_when(collapse ~ 1-(Bio_smry/collapseThresh)))
  
  smryBio <- tsBio %>% group_by(model_run, iteration, scenario) %>%
                summarize(yrsN = n(),
                          closuresFreq = sum(closure)/yrsN,
                          collapseFreq = sum(collapse)/yrsN,
                          bonanzaFreq = sum(bonanza)/yrsN,
                          meanB1plus = mean(Bio_smry),
                          meanCollapseSever = mean(collapseSever, na.rm = TRUE))
  
  maxRebuildLength <- tsBio %>% group_by(model_run, iteration, scenario, closure) %>%
                        summarize(rebuildLengthMax = max(closureLength)) %>%
                        filter(closure == TRUE)
  
  maxBonanzaLength <- tsBio %>% group_by(model_run, iteration, scenario, bonanza) %>%
                        summarize(bonanzaLengthMax = max(bonanzaLength)) %>%
                        filter(bonanza == TRUE)
  
  belowBonanza <- tsBio %>% group_by(model_run, iteration, scenario, bonanza) %>%
                    summarize(belowBonanzaLenMax = max(bonanzaLength),
                              belowBonanzaLenMean = mean(bonanzaLength)) %>%
                    filter(bonanza == FALSE)
  
  # catch metrics
  smryCat <- runSmryOutput$tsSmry %>% filter(model_run == omName, year > 2019) %>%
                mutate(totCatch = retainB_1 + retainB_2 + retainB_3) %>%
                group_by(year, model_run, iteration, scenario) %>%
                # summarize total catch within year
                dplyr::summarize(totCatch = sum(totCatch)) 
  smryCat <- smryCat %>% group_by(model_run, iteration, scenario) %>% 
                summarize(meanCatch = mean(totCatch),
                          sdCatch = sd(totCatch))
  
  # composition metrics
  
  # find columns for directory parsing
  dirsCol <- ncol(str_split(runSmryOutput$ageComp$resDir, 
                            pattern = "/", simplify = TRUE))
  
  smryAge <- runSmryOutput$ageComp %>% 
                # need to parse out iteration, model run and scenario
                mutate(model_run = str_split(resDir, pattern = "/", 
                                             simplify = TRUE)[, dirsCol], 
                       iteration = as.integer(str_split(resDir, pattern = "/", 
                                             simplify = TRUE)[, dirsCol-1]), 
                       scenario = str_split(resDir, pattern = "/", 
                                            simplify = TRUE)[, dirsCol-2]) %>%
                filter(grepl(omName, model_run, fixed = TRUE), Yr > 2019,
                       Seas == 1, Fleet == 4) %>%
                group_by(model_run, iteration, scenario) %>% 
                summarize(minAge = min(All_exp_mean))
  
  smryLen <- runSmryOutput$lenComp %>% 
                # need to parse out iteration, model run and scenario
                mutate(model_run = str_split(resDir, pattern = "/", 
                                             simplify = TRUE)[, dirsCol], 
                       iteration = as.integer(str_split(resDir, pattern = "/", 
                                             simplify = TRUE)[, dirsCol-1]), 
                       scenario = str_split(resDir, pattern = "/", 
                                            simplify = TRUE)[, dirsCol-2]) %>%
                filter(grepl(omName, model_run, fixed = TRUE), Yr > 2019,
                       Seas == 1, Fleet == 4) %>%
                group_by(model_run, iteration, scenario) %>% 
                summarize(minLen = min(All_exp_mean))
  
  metrics <- full_join(x = smryConvrg, y = smryBio, by = c("iteration", "scenario")) %>% # no model_run in smryConvrg because over EM runs
              full_join(y = maxRebuildLength, by = c("iteration", "model_run", "scenario")) %>%
              full_join(y = maxBonanzaLength, by = c("iteration", "model_run", "scenario")) %>%
              full_join(y = belowBonanza, by = c("iteration", "model_run", "scenario")) %>%
              full_join(y = smryCat, by = c("iteration", "model_run", "scenario")) %>%
              full_join(y = smryAge, by = c("iteration", "model_run", "scenario")) %>%
              full_join(y = smryLen, by = c("iteration", "model_run", "scenario"))
  
  return(list("perfomanceMetrics" = metrics,
              "tsSmry" = select(tsBio, -c(retainB_1, retainB_2, retainB_3))))
  }
