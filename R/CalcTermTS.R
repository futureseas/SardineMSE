# Calculate terminal year time series from the operating and estimation model output
# Created: 4/11/2022, Robert Wildermuth

CalcTermTS <- function(runSmryOutput,
                       termYr){

  omName <- unique(grep("_OM", runSmryOutput$tsSmry$model_run, 
                 fixed = TRUE, value = TRUE))
  initName <- unique(grep("_init", runSmryOutput$tsSmry$model_run, 
                   fixed = TRUE, value = TRUE))
  
  # biomass timeseries
  tsBio <- runSmryOutput$tsSmry %>% filter(Seas == 1) %>%
              mutate(emYear = as.numeric(regmatches(model_run,
                                                    gregexpr("[[:digit:]]+", 
                                                             model_run)))) 
  
  termTS <- tsBio %>% filter(!model_run %in% c(omName, initName),
                             year == emYear) %>%
              mutate(model_run = sub(pattern = "[[:digit:]]+", 
                                     replacement = "term", model_run))
  termTS <- rbind(termTS,
                  tsBio %>% filter(model_run %in% c(omName, initName))) %>%
              arrange(scenario, iteration, year) %>%
              select(-c(retainB_1, retainB_2, retainB_3))
  
  # catch timeseries
  tsTotCat <- runSmryOutput$tsSmry %>% 
                mutate(totCatch = retainB_1 + retainB_2 + retainB_3) %>%
                group_by(year, model_run, iteration, scenario) %>%
                # summarize total catch within year
                dplyr::summarize(totCatch = sum(totCatch)) %>%
                mutate(emYear = as.numeric(regmatches(model_run,
                                                      gregexpr("[[:digit:]]+", 
                                                               model_run))))
  
  termCat <- tsTotCat %>% filter(!model_run %in% c(omName, initName),
                                 year == emYear) %>%
                mutate(model_run = sub(pattern = "[[:digit:]]+", 
                                       replacement = "term", model_run))
  termCat <- rbind(termCat,
                   tsTotCat %>% filter(model_run %in% c(omName, initName))) %>%
                arrange(scenario, iteration, year)
  
  # recruitment timeseries
  tsRec <- runSmryOutput$dqSmry %>% 
              mutate(emYear = as.numeric(regmatches(model_run,
                                                    gregexpr("[[:digit:]]+", 
                                                             model_run))))
  
  termRec <- tsRec %>% filter(!model_run %in% c(omName, initName),
                              year == emYear) %>%
                mutate(model_run = sub(pattern = "[[:digit:]]+", 
                                       replacement = "term", model_run))
  termRec <- rbind(termRec,
                   tsRec %>% filter(model_run %in% c(omName, initName))) %>%
                arrange(scenario, iteration, year)
  
  termTS <- full_join(termTS, termCat, by = c("year", "model_run", "iteration", "scenario")) %>%
              full_join(y = termRec, by = c("year", "model_run", "iteration", "scenario"))
  # endTime <- Sys.time()
  
  termTS <- termTS %>% select(Bio_smry, rec_dev, year, Seas, model_run, iteration,
                              scenario, totCatch, Value.Recr, Value.SSB, emYear)

  return(termTS)
}