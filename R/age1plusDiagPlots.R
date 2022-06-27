# Function to plot OM and EM estimated Age1+ for sardine SS models
# Created: 9/2/2021; Robert Wildermuth

age1plusDiagPlots <- function(dir, # SSMSE directory (character)
                              scenario, # scenario name (character)
                              termYr # terminal year of MSE run
){
  
  # get the iterations
  iters <- list.dirs(file.path(dir, scenario), recursive = FALSE, full.names = FALSE)
  
  # # get the OM, init, and terminal year model directory names
  omName <- grep("_OM", list.dirs(file.path(dir, scenario, iters[1]),
                                  recursive = FALSE,
                                  full.names = FALSE), value = TRUE)
  initName <- grep("_init", list.dirs(file.path(dir, scenario, iters[1]),
                                      recursive = FALSE,
                                      full.names = FALSE), value = TRUE)
  
  termNames <- list.dirs(file.path(dir, scenario, iters[1]),
                         recursive = FALSE,
                         full.names = FALSE)
  termNames <- termNames[which(!termNames %in% c(omName, initName))]
  
  # Extract terminal summary biomass for OM and EM by iteration
  smryBio <- data.frame()
  for(i in iters){
    
    # RW: Don't need to pull from ss_summary file
    # emSummary <- readLines(con = file.path(dir, scenario, i, termName, "ss_summary.sso"), warn = FALSE)
    # 
    # emSummary[grep("#_Biomass", emSummary, fixed = TRUE):length(emSummary)]                   
    # emSummary[grepl("SmryBio_", emSummary, fixed = TRUE)]
    # 
    # 
    # get.vec <- function(dat, ind) {
    #   ## Returns the next vector of numbers in the dat vector.
    #   ## Increments ind in the parent environment.
    #   assign("ind", ind + 1, parent.frame())
    #   ## Split by whitespace and collapse (+).
    #   vec <- strsplit(dat[ind], "[[:blank:]]+")
    #   as.numeric(vec[[1]])
    
    initOutputSS <- SS_output(dir = file.path(dir, scenario, i, initName),
                              dir.mcmc = NULL,
                              repfile = "Report.sso",
                              compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
    
    # Get EM estimated Age1+
    smryEM <- initOutputSS[["sprseries"]] %>% select(Yr, Era, Bio_Smry.1) %>%
      mutate(model_run = paste0(scenario, "_EM_term"),
             iteration = as.integer(i))
    
    # Next get the forecast summary age 1+ biomass for each projected EM year
    for(y in max(smryEM$Yr):termYr){
      outputSS <- SS_output(dir = file.path(dir, scenario, i, 
                                            grep(y, termNames, value = TRUE)),
                            dir.mcmc = NULL,
                            repfile = "Report.sso",
                            compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
      
      nxtYrAge1Plus <- outputSS[["sprseries"]] %>% select(Yr, Era, Bio_Smry.1) %>%
        mutate(model_run = paste0(scenario, "_EM_term"),
               iteration = as.integer(i)) %>%
        filter(Yr == y+1)
      
      # add this to the EM time series
      smryEM <- rbind(smryEM, nxtYrAge1Plus)
    }
    
    OMoutputSS <- SS_output(dir = file.path(dir, scenario, i, omName),
                            dir.mcmc = NULL,
                            repfile = "Report.sso",
                            compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
    
    # Get OM Age1+
    # 'sprseries' has the annual values, 'timeseries' has by season
    smryOM <- OMoutputSS[["sprseries"]] %>% select(Yr, Era, Bio_Smry.1) %>%
      filter(Era == "TIME") %>% 
      mutate(model_run = omName,
             iteration = as.integer(i))
    
    smryBio <- rbind(smryBio, smryEM, smryOM)
  }
  
  absoluteSmry <- ggplot2::ggplot(data = smryBio, 
                                  ggplot2::aes(x = Yr, y = Bio_Smry.1)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
    ggplot2::scale_color_manual(values = c("#D65F00", "blue")) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic()
  
  # Calculate Summary biomass relative error
  relSmry_EMtoOM <- smryBio %>% select(model_run, Yr, iteration, Bio_Smry.1) %>%
    pivot_wider(id_cols = c(Yr, iteration),
                names_from = model_run, values_from = Bio_Smry.1) %>%
    rename(OM = all_of(omName),
           EM = all_of(paste0(scenario, "_EM_term"))) %>% 
    mutate(errSmryBio = (EM - OM)/OM)
  
  relSmry <- ggplot(data = relSmry_EMtoOM, aes(x = Yr, y = errSmryBio)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    geom_hline(yintercept = 0, color = "black") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = 2))+
    #ggplot2::scale_color_manual(values = c("#D65F00", "black", rep("blue", 5))) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic() +
    ylab("Relative Error (Summary Biomass)")
  
  list(absoluteSmry, relSmry)
}
