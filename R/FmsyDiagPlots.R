# Function to plot Fmsy from OM and EM 
# Created: 9/2/2021; Robert Wildermuth

# Note: assumes SSMSE_summary_all has been run for a completed scenario
FmsyDiagPlots <- function(dir, # SSMSE directory (character)
                          scenario, # scenario name (character)
                          termYr # terminal year of MSE run
){
  # read in SSMSE results summary derived quantaties
  sclSumry <- read.csv(file.path(dir, scenario, paste0("results_scalar_", scenario, ".csv")))
  
  # get the OM, init, and terminal year model directory names
  omName <- grep("_OM", sclSumry$model_run, value = TRUE)[1]
  initName <- grep("_init", sclSumry$model_run, value = TRUE)[1]
  termName <- grep(paste0("_", termYr), sclSumry$model_run, value = TRUE)[1]
  
  ggplot2::ggplot(data = sclSumry, #subset(sclSumry, model_run %in% c(omName, termName, initName)), 
                  ggplot2::aes(x = model_run, y = F_MSY)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}