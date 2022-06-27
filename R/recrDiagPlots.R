# Function to plot recruitment from OM and EM 
# Created: 9/2/2021; Robert Wildermuth

# Note: assumes SSMSE_summary_all has been run for a completed scenario

recrDiagPlots <- function(dir, # SSMSE directory (character)
                          scenario, # scenario name (character)
                          termYr # terminal year of MSE run
){
  # read in SSMSE results summary derived quantaties
  dqSumry <- read.csv(file.path(dir, scenario, paste0("results_dq_", scenario, ".csv")))
  
  # get the OM, init, and terminal year model directory names
  omName <- grep("_OM", dqSumry$model_run, value = TRUE)[1]
  initName <- grep("_init", dqSumry$model_run, value = TRUE)[1]
  termName <- grep(paste0("_", termYr), dqSumry$model_run, value = TRUE)[1]
  
  # Find the estimate of the terminal year for each year of the MSE cycle
  termYr <- dqSumry %>% select(Value.Recr, year, model_run, iteration) %>%
    filter(model_run %in% c(omName, initName) | # or keep end years
             year == as.numeric(regmatches(model_run, 
                                           gregexpr("[[:digit:]]+", model_run)))) %>% 
    mutate(model_run = as.character(model_run),
           model_run = case_when(!model_run %in% c(omName, initName) ~ paste0(scenario, "_EM_term"),
                                 TRUE ~ model_run))
  
  absoluteRec <- ggplot2::ggplot(data = termYr, 
                                 ggplot2::aes(x = year, y = Value.Recr)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
    ggplot2::scale_color_manual(values = c("#D65F00", "black", "blue")) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic()
  
  # Calculate recruitment relative error
  relRec_EMtoOM <- termYr %>% pivot_wider(id_cols = c(year, iteration),
                                          names_from = model_run, values_from = Value.Recr) %>%
    rename(OM = all_of(omName))
  
  # crazy BS to make a summary column based on conditions in another column
  relRec_EMtoOM <- relRec_EMtoOM %>% add_column(diffRec = unlist(relRec_EMtoOM[, initName]))
  histInd <- which(!is.na(relRec_EMtoOM[,paste0(scenario, "_EM_term")]))
  relRec_EMtoOM[histInd, "diffRec"] <- relRec_EMtoOM[histInd, paste0(scenario, "_EM_term")]
  relRec_EMtoOM <- relRec_EMtoOM %>% mutate(diffRec = (diffRec - OM)/OM)
  
  relRec <- ggplot(data = relRec_EMtoOM, aes(x = year, y = diffRec)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    geom_hline(yintercept = 0, color = "black") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = 2))+
    #ggplot2::scale_color_manual(values = c("#D65F00", "black", rep("blue", 5))) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic() +
    ylab("Relative Error (Recruitment)")
  
  list(absoluteRec, relRec)
  
}