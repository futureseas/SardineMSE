# Function to plot OM, simulated data, and EM estimated catch for sardine SS models
# Created: 9/2/2021; Robert Wildermuth

catchDiagPlots <- function(dir, # SSMSE directory (character)
                           scenario, # scenario name (character)
                           termYr # terminal year of MSE run
){
  
  # get the iterations
  iters <- list.dirs(file.path(dir, scenario), recursive = FALSE, full.names = FALSE)
  
  # # get the OM, init, and terminal year model directory names
  omName <- grep("_OM", list.dirs(file.path(dir, scenario, iters[1]),
                                  recursive = FALSE,
                                  full.names = FALSE), value = TRUE)
  # initName <- grep("_init", list.dirs(file.path(dir, scenario, iters[1]),
  #                                 recursive = FALSE,
  #                                 full.names = FALSE), value = TRUE)
  termName <- grep(paste0("_", termYr),
                   list.dirs(file.path(dir, scenario, iters[1]),
                             recursive = FALSE,
                             full.names = FALSE), value = TRUE)
  
  # Extract terminal age and length comps for OM and EM by iteration
  catch <- data.frame()
  simDat <- data.frame()
  for(i in iters){
    outputSS <- SS_output(dir = file.path(dir, scenario, i, termName),
                          dir.mcmc = NULL,
                          repfile = "Report.sso",
                          compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
    
    # Get EM estimated catch
    catchEM <- outputSS[["catch"]] %>% group_by(Yr) %>%
      # summarize total catch within year
      dplyr::summarize(obsCat = sum(Obs),
                       expCat = sum(Exp)) %>%
      mutate(model_run = termName,
             iteration = as.integer(i))
    
    # get OM catch
    OMoutputSS <- SS_output(dir = file.path(dir, scenario, i, omName),
                            dir.mcmc = NULL,
                            repfile = "Report.sso",
                            compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
    
    catchOM <- OMoutputSS[["catch"]] %>% group_by(Yr) %>%
      # summarize totoal catch within year
      dplyr::summarize(obsCat = sum(Obs),
                       expCat = sum(Exp)) %>%
      mutate(model_run = omName,
             iteration = as.integer(i))
    
    itDat <- SS_readdat(file = file.path(dir, scenario, i, termName, "data.ss_new"),
                        version = "3.30")[["catch"]] %>%
      # summarize totoal catch within year
      group_by(year) %>%
      dplyr::summarize(catch = sum(catch)) %>%
      mutate(iteration = as.integer(i),
             model_run = paste0(scenario,"_data"))
    
    catch <- rbind(catch, catchEM, catchOM)
    simDat <- rbind(simDat, itDat)
  } # end 'iter' for-loop
  
  # catch <- catch %>% select(Yr, expCat, model_run, iteration) %>%
  #             rename(year = Yr,
  #                    catch = expCat)
  simDat <- simDat %>% filter(year > 0)
  #catch <- rbind(catch, simDat)
  
  absoluteCatch <- ggplot2::ggplot(data = catch, 
                                   ggplot2::aes(x = Yr, y = expCat)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    geom_point(data = simDat, mapping = aes(x = year, y = catch), shape = 4, color = "grey") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
    ggplot2::scale_color_manual(values = c("#D65F00", "blue")) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = FALSE) +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic()
  
  # Calculate catch relative error
  relCat_EMtoOM <- catch %>% select(model_run, Yr, iteration, expCat) %>%
    pivot_wider(id_cols = c(Yr, iteration),
                names_from = model_run, values_from = expCat) %>%
    rename(OM = all_of(omName),
           EM = all_of(termName)) %>% 
    mutate(errCat = (EM - OM)/OM)
  
  # Merge the simulated data with the model estimates
  relCat_EMtoOM <- relCat_EMtoOM %>% full_join(y = simDat, by = c("Yr" = "year", "iteration")) %>%
    mutate(diffDat = (catch - OM)/OM)
  
  relCatch <- ggplot(data = relCat_EMtoOM, aes(x = Yr, y = errCat)) +
    ggplot2::geom_vline(xintercept = 2019, color = "gray") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(mapping = aes(x = Yr, y = diffDat), shape = 4, color = "grey") +
    ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = 2))+
    #ggplot2::scale_color_manual(values = c("#D65F00", "black", rep("blue", 5))) +
    ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
    ggplot2::guides(linetype = "none") +
    ggplot2::facet_wrap(. ~ iteration) +
    ggplot2::theme_classic() +
    ylab("Relative Error (Catch)")
  
  list(absoluteCatch, relCatch)
}
