# Fxn to calculate iteration-level mean and median rebuilding time from SSMSE scenario output
# Created: 12/5/2022, Robert Wildermuth

CalcRebuildTime <- function(tsSmry){
  rebuildTime <- tsSmry %>% select(Bio_smry, year, model_run, iteration, 
                                   scenario, recScen, HCR, closure, closureLength) %>%
    filter(closure) %>% arrange(scenario, iteration, year) #%>% filter(year == 2020)
  #all(rebuildTime$closure)
  rebuildTime$isRebuilt <- NA
  for(i in 1:(nrow(rebuildTime)-1)){
    rebuildTime$isRebuilt[i] <- if(rebuildTime$closureLength[i] != rebuildTime$closureLength[i+1]-1){
      TRUE
    } else { FALSE }
  }
  rebuildTime <- rebuildTime %>% filter(isRebuilt, year != 2069) # remove last year b/c can't guarantee stock is rebuilt
  
  rebuildTime <- rebuildTime %>% group_by(model_run, iteration, scenario, HCR, recScen) %>%
                    summarize(nClosures = n(),
                              meanRebuildTime = mean(closureLength),
                              medRebuildTime = median(closureLength)) 
  
  collapseTime <- tsSmry %>% select(Bio_smry, year, model_run, iteration, 
                                   scenario, recScen, HCR, collapse, collapseLength) %>%
    filter(collapse) %>% arrange(scenario, iteration, year) #%>% filter(year == 2020)

  collapseTime$notCollapsed <- NA
  for(i in 1:(nrow(collapseTime)-1)){
    collapseTime$notCollapsed[i] <- if(collapseTime$collapseLength[i] != collapseTime$collapseLength[i+1]-1){
      TRUE
    } else { FALSE }
  }
  collapseTime <- collapseTime %>% filter(notCollapsed, year != 2069) # remove last year b/c can't guarantee stock is rebuilt
  
  collapseTime <- collapseTime %>% group_by(model_run, iteration, scenario, HCR, recScen) %>%
    summarize(nCollapses = n(),
              meanCollapseTime = mean(collapseLength),
              medCollapseTime = median(collapseLength)) 
  
  rebuildTime <- rebuildTime %>% full_join(y = collapseTime,
                                           by = c("model_run", "iteration", "scenario", 
                                                  "HCR", "recScen"))
  
  return(rebuildTime)
}