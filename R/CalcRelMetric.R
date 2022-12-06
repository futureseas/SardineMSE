# Fxn to calculate relative biomass or catch within recruitment scenario wrt an input dataset
# Created: 12/5/2022, Robert Wildermuth

CalcRelMetric <- function(metricData, # Data to calculate relative metric for
                          metricCol, # Name of column to take relative metric of
                          optnlRefDat, # optional reference data to use as denominator
                          refCol = NULL, # Name of column for reference data
                          useMax = FALSE # use the maximum of reference in denominator?
                          ){
  # if optional reference data provided, use that for denominator
  if(missing(optnlRefDat)){
    relDenoms <- metricData %>% group_by(recScen, iteration) %>%
                    summarize(maxMetric = max(get(metricCol)),
                              meanMetric = mean(get(metricCol)))
    
  } else {
    relDenoms <- optnlRefDat %>% group_by(recScen, iteration) %>%
                    summarize(maxMetric = max(get(refCol)),
                              meanMetric = mean(get(refCol)))
  }
  
  if(useMax){
    metricData <- metricData %>% full_join(y = relDenoms,
                                           by = c("recScen", "iteration")) %>%
                    mutate(relMetricMax = get(metricCol)/maxMetric) %>%
                    rename_with(.fn = ~paste0("rel", metricCol, "Max"),
                                .cols = "relMetricMax")
  } else {
    metricData <- metricData %>% full_join(y = relDenoms,
                                           by = c("recScen", "iteration")) %>%
                    mutate(relMetricMean = get(metricCol)/meanMetric) %>%
                    rename_with(.fn = ~paste0("rel", metricCol, "Mean"), 
                                .cols = "relMetricMean")
  }
    
  return(metricData)
}
