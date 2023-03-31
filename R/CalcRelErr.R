# Fxn to calculate yearly relative error from SSMSE scenario output
# Created: 12/5/2022, Robert Wildermuth

CalcRelErr <- function(smryOutputList,
                       termTS, # df from CalcTermTS()
                       termYr # last year of EM simulation
                       ){
  
  omName <- grep("_OM", smryOutputList$tsSmry$model_run,
                 fixed = TRUE, value = TRUE)[1]
  
  convrgCheck <- smryOutputList$sclSmry %>% 
                    select(max_grad, model_run, iteration, scenario, HCR, recScen) %>%
                    mutate(emYear = as.numeric(regmatches(model_run,
                                                          gregexpr("[[:digit:]]+", 
                                                                   model_run))))
  
  cnvrgTS <- smryOutputList$tsSmry  %>%
                left_join(y = convrgCheck, by = c("iteration", "model_run", 
                                                  "scenario", "HCR", "recScen")) %>%
                mutate(plotGroup = case_when(model_run == omName ~ "OM",
                                             max_grad > 0.01 ~ "non-convrg",
                                             max_grad < 0.01 ~ "convrg"))

  # Bring in simulated data series used for HCR9 biomass ests
  simBioObsHCR9 <- smryOutputList$obsCPUE %>%
                      mutate(emYear = as.numeric(regmatches(model_run,
                                                            gregexpr("[[:digit:]]+", 
                                                                     model_run))),
                             plotGroup = "ATsurvey") %>%
                      # need to filter to data used in final assessment per MSE run
                      filter(emYear == termYr, index == 4, year > 2019, HCR == "HCR9") %>%
                      select(year, obs, emYear, model_run, iteration, scenario, HCR, 
                             recScen, plotGroup) %>%
                      rename(Bio_smry = obs)

  errCompare <- cnvrgTS %>% filter(Seas == 1, model_run != omName) %>%
                  select(Bio_smry, year, model_run, iteration, scenario, HCR, 
                         recScen, emYear, plotGroup) %>%
                  bind_rows(simBioObsHCR9) %>%
                  inner_join(y = subset(termTS, model_run == omName), 
                             by = c("year", "iteration", "scenario", "HCR", "recScen")) %>%
                  rename(age1plusOM = Bio_smry.y,
                         age1plusEM = Bio_smry.x) %>% 
                  mutate(errSmryBio = ((age1plusEM - age1plusOM)/age1plusOM)*100,
                         recDevState = case_when(rec_dev <= -1.25 ~ "poor",
                                                 rec_dev >= 1.25 ~ "high",
                                                 TRUE ~ "avg"))
  
  return(errCompare)
}

