# Function to plot absolute spawning biomass from OM, simulated data, 
# and EM in and also relative error
# Created: 9/2/2021; Robert Wildermuth

# Note: assumes SSMSE_summary_all has been run for a completed scenario

bDiagPlots <- function(dir, # SSMSE directory (character)
                       scenario, # scenario name (character)
                       termYr, # terminal year of MSE run
                       surveyInx # numeric index of the survey fleet for data comparison (e.g., Acoustic-Trawl)
){
  
  # read in SSMSE results summary
  tsSumry <- read.csv(file.path(dir, scenario, paste0("results_ts_", scenario, ".csv")))
  
  # Get rid of duplicated SSB years
  tsSumry <- tsSumry %>% select(SpawnBio, year, iteration, model_run)
  tsSumry <- na.omit(tsSumry)
  
  # get the iterations
  iters <- list.dirs(file.path(dir, scenario), recursive = FALSE, full.names = FALSE)
  
  # get the OM, init, and terminal year model directory names
  omName <- grep("_OM", list.dirs(file.path(dir, scenario, iters[1]),
                                  recursive = FALSE,
                                  full.names = FALSE), value = TRUE)
  initName <- grep("_init", list.dirs(file.path(dir, scenario, iters[1]),
                                      recursive = FALSE,
                                      full.names = FALSE), value = TRUE)
  termName <- grep(paste0("_", termYr), 
                   list.dirs(file.path(dir, scenario, iters[1]),
                             recursive = FALSE,
                             full.names = FALSE), value = TRUE)
  
  # bring in simulated data for each iteration to plot with OM and EM vals
  simDat <- data.frame()
  for(i in iters){
    itDat <- SS_readdat(file = file.path(dir, scenario, i, termName, "data.ss_new"),
                        version = "3.30")[["CPUE"]] %>%
      filter(index == surveyInx) %>%
      mutate(iteration = i,
             model_run = paste0(scenario,"_data"))
    
    simDat <- rbind(simDat, itDat)
  }
  
  # Find the estimate of the terminal year for each year of the MSE cycle
  termYr <- tsSumry %>% filter(model_run %in% c(omName, initName) | # or keep end years
                                 year == as.numeric(regmatches(model_run, 
                                                               gregexpr("[[:digit:]]+", model_run)))) %>% 
    mutate(model_run = as.character(model_run),
           model_run = case_when(!model_run %in% c(omName, initName) ~ paste0(scenario, "_EM_term"),
                                 TRUE ~ model_run))
  
  # plot biomass per iteration
  absoluteBio <- ggplot2::ggplot(data = termYr, 
                                 ggplot2::aes(x = year, y = SpawnBio)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    geom_point(data = simDat, mapping = aes(x = year, y = obs), shape = 4, color = "grey") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
    ggplot2::scale_color_manual(values = c("#D65F00", "black", rep("blue", 5))) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic()
  
  # Calculate SSB relative error
  relSSB_EMtoOM <- termYr %>% select(model_run, year, iteration, SpawnBio) %>%
    pivot_wider(id_cols = c(year, iteration),
                names_from = model_run, values_from = SpawnBio) %>%
    rename(OM = all_of(omName))
  
  # crazy BS to make a summary column based on conditions in another column
  relSSB_EMtoOM <- relSSB_EMtoOM %>% add_column(diffSSB = unlist(relSSB_EMtoOM[, initName]))
  histInd <- which(!is.na(relSSB_EMtoOM[,paste0(scenario, "_EM_term")]))
  relSSB_EMtoOM[histInd, "diffSSB"] <- relSSB_EMtoOM[histInd, paste0(scenario, "_EM_term")]
  relSSB_EMtoOM <- relSSB_EMtoOM %>% mutate(diffSSB = (diffSSB - OM)/OM)
  
  # Merge the simulated data with the model estimates
  simDat <- simDat %>% convert(int(iteration))
  relSSB_EMtoOM <- relSSB_EMtoOM %>% full_join(y = simDat, by = c("year", "iteration")) %>%
    mutate(diffDat = (obs - OM)/OM)
  
  relBio <- ggplot(data = relSSB_EMtoOM, aes(x = year, y = diffSSB)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(mapping = aes(x = year, y = diffDat), shape = 4, color = "grey") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = 2))+
    #ggplot2::scale_color_manual(values = c("#D65F00", "black", rep("blue", 5))) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic() +
    ylab("Relative Error (SSB)")
  
  list(absoluteBio, relBio)
}
