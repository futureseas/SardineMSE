# Function to plot length and age composition plots for sardine SS models
# Created: 9/2/2021; Robert Wildermuth

# Note: Simulated age and length comp data are scaled by EM estimated number of
#       Age 1+ sardines (sprseries$Num_Smry).

compDiagPlots <- function(dir, # SSMSE directory (character)
                          scenario, # scenario name (character)
                          termYr, # terminal year of MSE run
                          surveyInx = 4, # numeric index for the survey fleet (default AT survey)
                          biomass = TRUE, # report comps as biomass? Default is TRUE
                          rawVals = FALSE # include raw values in composition plots? Default is proportions
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
  termLenComps <- data.frame()
  termAgeComps <- data.frame()
  # add in the simulated comps data
  simAgeDat <- data.frame()
  simLenDat <- data.frame()
  
  if(biomass){
    for(i in iters){
      outputSS <- SS_output(dir = file.path(dir, scenario, i, termName),
                            dir.mcmc = NULL,
                            repfile = "Report.sso",
                            compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
      
      # Get estimation model's estimated compositions
      EMLenComps <- outputSS[["batlen"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
        #                       "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
        #                       "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
        #                       "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5", 
        #                       "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
        #              names_to = "Bin", values_to = "Biomass") %>%
        mutate(#Bin = as.numeric(Bin),
               model_run = termName,
               iteration = as.integer(i))
      
      EMAgeComps <- outputSS[["batage"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        #              names_to = "Age", values_to = "Biomass") %>%
        mutate(#Age = as.numeric(Age),
               model_run = termName,
               iteration = as.integer(i))
      
      # get OM compositions
      OMoutputSS <- SS_output(dir = file.path(dir, scenario, i, omName),
                              dir.mcmc = NULL,
                              repfile = "Report.sso",
                              compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
      
      OMLenComps <- OMoutputSS[["batlen"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
        #                       "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
        #                       "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
        #                       "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5", 
        #                       "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
        #              names_to = "Bin", values_to = "Biomass") %>%
        mutate(#Bin = as.numeric(Bin),
               model_run = omName,
               iteration = as.integer(i))
      OMAgeComps <- OMoutputSS[["batage"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        #              names_to = "Age", values_to = "Biomass") %>%
        mutate(#Age = as.numeric(Age),
               model_run = omName,
               iteration = as.integer(i))
      termAgeComps <- rbind(termAgeComps, EMAgeComps, OMAgeComps)
      termLenComps <- rbind(termLenComps, EMLenComps, OMLenComps)
      
    }
  
    # Calculate proportions and pivot for plotting
    if(rawVals){ 
      termLenComps <- termLenComps %>% 
                        pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
                                              "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
                                              "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
                                              "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5",
                                              "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
                                     names_to = "Bin", values_to = "Biomass") %>%
                        mutate(Bin = as.numeric(Bin))
      
      
      termAgeComps <- termAgeComps %>% 
                        pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                     names_to = "Age", values_to = "Biomass") %>%
                        mutate(Age = as.numeric(Age))
          
      
    } else {
      termLenComps <- termLenComps %>% rowwise() %>%
                        mutate(total = sum(c(`6`, `6.5`, `7`, `7.5`, `8`, `8.5`, `9`, `9.5`, `10`, `10.5`, 
                                             `11`, `11.5`, `12`, `12.5`, `13`, `13.5`, `14`, `14.5`, `15`, `15.5`,
                                             `16`, `16.5`, `17`, `17.5`, `18`, `18.5`, `19`, `19.5`, `20`, `20.5`,
                                             `21`, `21.5`, `22`, `22.5`, `23`, `23.5`, `24`, `24.5`, `25`, `25.5`,
                                             `26`, `26.5`, `27`, `27.5`, `28`, `28.5`, `29`, `29.5`, `30`))) %>%
                        pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
                                              "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
                                              "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
                                              "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5",
                                              "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
                                     names_to = "Bin", values_to = "Biomass") %>%
                        mutate(Biomass = Biomass/total,
                               Bin = as.numeric(Bin))
      
      termAgeComps <- termAgeComps %>% rowwise() %>%
                        mutate(total = sum(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`))) %>%
                        pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                     names_to = "Age", values_to = "Biomass") %>%
                        mutate(Biomass = Biomass/total,
                               Age = as.numeric(Age))           
    }
    
    # Plot the comps by model and iteration
    lenCompPlot <- ggplot(termLenComps, mapping = aes(x = Bin, y = Biomass)) +
      geom_col() +
      facet_grid(cols = vars(model_run), rows = vars(iteration))
    
    ageCompPlot <- ggplot(termAgeComps, mapping = aes(x = Age, y = Biomass)) +
      geom_col() +
      facet_grid(cols = vars(model_run), rows = vars(iteration))
    
  } else{  
    for(i in iters){
      outputSS <- SS_output(dir = file.path(dir, scenario, i, termName),
                            dir.mcmc = NULL,
                            repfile = "Report.sso",
                            compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
      
      # Get estimation model's estimated compositions
      EMLenComps <- outputSS[["natlen"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
        #                       "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
        #                       "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
        #                       "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5", 
        #                       "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
        #              names_to = "Bin", values_to = "Number") %>%
        mutate(#Bin = as.numeric(Bin),
               model_run = termName,
               iteration = as.integer(i))
      
      EMAgeComps <- outputSS[["natage"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        #              names_to = "Age", values_to = "Number") %>%
        mutate(#Age = as.numeric(Age),
               model_run = termName,
               iteration = as.integer(i))
      
      # get OM compositions
      OMoutputSS <- SS_output(dir = file.path(dir, scenario, i, omName),
                              dir.mcmc = NULL,
                              repfile = "Report.sso",
                              compfile = "CompReport.sso", verbose = FALSE, printstats = FALSE)
      
      OMLenComps <- OMoutputSS[["natlen"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
        #                       "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
        #                       "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
        #                       "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5", 
        #                       "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
        #              names_to = "Bin", values_to = "Number") %>%
        mutate(#Bin = as.numeric(Bin),
               model_run = omName,
               iteration = as.integer(i))
      OMAgeComps <- OMoutputSS[["natage"]] %>% filter(Yr == termYr, Seas == 2, `Beg/Mid` == "B") %>%
        # pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        #              names_to = "Age", values_to = "Number") %>%
        mutate(#Age = as.numeric(Age),
               model_run = omName,
               iteration = as.integer(i))
      termAgeComps <- rbind(termAgeComps, EMAgeComps, OMAgeComps)
      termLenComps <- rbind(termLenComps, EMLenComps, OMLenComps)
      
      
      # pull in the simulated age and length data
      itAgeDat <- SS_readdat(file = file.path(dir, scenario, i, termName, "data.ss_new"),
                             version = "3.30")[["agecomp"]] %>%
        filter(FltSvy == surveyInx,
               Yr == termYr) %>%
        # pivot_longer(cols = c("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"),
        #              names_to = "aNum", values_to = "Number") %>%
        mutate(#Age = as.numeric(gsub(pattern = "a", replacement = "", aNum)),
               iteration = as.integer(i),
               model_run = paste0(scenario,"_data"))
      
      itLenDat <- SS_readdat(file = file.path(dir, scenario, i, termName, "data.ss_new"),
                             version = "3.30")[["lencomp"]] %>%
        filter(FltSvy == surveyInx,
               Yr == termYr)
      itLenDat <- itLenDat %>% #pivot_longer(cols = grep("^[l][0-9]", names(itLenDat)), # pulls the l columns followed by a number
                                #            names_to = "Length", values_to = "Number") %>%
        mutate(#Bin = as.numeric(gsub(pattern = "l", replacement = "", Length)),
               iteration = as.integer(i),
               model_run = paste0(scenario,"_data"))
      
      # scale the sampled data by estimated number of Age1+ fish
      numScale <- outputSS[["sprseries"]] %>% filter(Yr == termYr) %>% select(Num_Smry)
      
      itAgeDat <- itAgeDat %>% mutate(numScale = as.numeric(numScale))#Number = (Number * as.numeric(numScale))/Nsamp)
      itLenDat <- itLenDat %>% mutate(numScale = as.numeric(numScale))# Number = (Number * as.numeric(numScale))/Nsamp)
      
      simAgeDat <- rbind(simAgeDat, itAgeDat)
      simLenDat <- rbind(simLenDat, itLenDat)
    } #end 'iters' for-loop
    
    # Calculate proportions and pivot for plotting
    if(rawVals){ 
      termLenComps <- termLenComps %>% 
        pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
                              "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
                              "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
                              "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5",
                              "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
                     names_to = "Bin", values_to = "Number") %>%
        mutate(Bin = as.numeric(Bin))
      
      termAgeComps <- termAgeComps %>% 
        pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     names_to = "Age", values_to = "Number") %>%
        mutate(Age = as.numeric(Age))
      
      simAgeDat <- simAgeDat %>%
        pivot_longer(cols = c("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"),
                     names_to = "aNum", values_to = "Number") %>%
        mutate(Age = as.numeric(gsub(pattern = "a", replacement = "", aNum)))
          
      simLenDat <- simLenDat %>% pivot_longer(cols = grep("^[l][0-9]", names(simLenDat)), # pulls the l columns followed by a number
                                              names_to = "Length", values_to = "Number") %>%
                      mutate(Bin = as.numeric(gsub(pattern = "l", replacement = "", Length)))
          
      # scale the sampled data by estimated number of Age1+ fish
      simAgeDat <- simAgeDat %>% mutate(Number = (Number * numScale)/Nsamp)
      simLenDat <- simLenDat %>% mutate(Number = (Number * numScale)/Nsamp)
      
    } else {
      termLenComps <- termLenComps %>% rowwise() %>%
        mutate(total = sum(c(`6`, `6.5`, `7`, `7.5`, `8`, `8.5`, `9`, `9.5`, `10`, `10.5`, 
                             `11`, `11.5`, `12`, `12.5`, `13`, `13.5`, `14`, `14.5`, `15`, `15.5`,
                             `16`, `16.5`, `17`, `17.5`, `18`, `18.5`, `19`, `19.5`, `20`, `20.5`,
                             `21`, `21.5`, `22`, `22.5`, `23`, `23.5`, `24`, `24.5`, `25`, `25.5`,
                             `26`, `26.5`, `27`, `27.5`, `28`, `28.5`, `29`, `29.5`, `30`))) %>%
        pivot_longer(cols = c("6", "6.5", "7", "7.5", "8", "8.5", "9", "9.5", "10", "10.5", 
                              "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5",
                              "16", "16.5", "17", "17.5", "18", "18.5", "19", "19.5", "20", "20.5",
                              "21", "21.5", "22", "22.5", "23", "23.5", "24", "24.5", "25", "25.5",
                              "26", "26.5", "27", "27.5", "28", "28.5", "29", "29.5", "30"),
                     names_to = "Bin", values_to = "Number") %>%
        mutate(Number = Number/total,
               Bin = as.numeric(Bin))
      
      termAgeComps <- termAgeComps %>% rowwise() %>%
        mutate(total = sum(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`))) %>%
        pivot_longer(cols = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     names_to = "Age", values_to = "Number") %>%
        mutate(Number = Number/total,
               Age = as.numeric(Age)) 
      
      simAgeDat <- simAgeDat %>%
        pivot_longer(cols = c("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"),
                     names_to = "aNum", values_to = "Number") %>%
        mutate(Age = as.numeric(gsub(pattern = "a", replacement = "", aNum)),
               Number = Number/Nsamp)
      
      simLenDat <- simLenDat %>% pivot_longer(cols = grep("^[l][0-9]", names(simLenDat)), # pulls the l columns followed by a number
                                              names_to = "Length", values_to = "Number") %>%
        mutate(Bin = as.numeric(gsub(pattern = "l", replacement = "", Length)),
               Number = Number/Nsamp)
      
    }
    
    # bind the simulated and modeled comps together for plotting
    termLenComps <- rbind(termLenComps[, c("Yr", "Seas", "Bin", "Number", "model_run", "iteration")],
                          simLenDat[, c("Yr", "Seas", "Bin", "Number", "model_run", "iteration")])
    
    termAgeComps <- rbind(termAgeComps[, c("Yr", "Seas", "Age", "Number", "model_run", "iteration")],
                          simAgeDat[, c("Yr", "Seas", "Age", "Number", "model_run", "iteration")])
    
    # Plot the comps by model and iteration
    lenCompPlot <- ggplot(termLenComps, mapping = aes(x = Bin, y = Number)) +
      geom_col() +
      facet_grid(cols = vars(model_run), rows = vars(iteration))
    
    ageCompPlot <- ggplot(termAgeComps, mapping = aes(x = Age, y = Number)) +
      geom_col() +
      facet_grid(cols = vars(model_run), rows = vars(iteration))
  } # end 'biomass' if-else
  
  
  
  list(lenCompPlot, ageCompPlot)
}
