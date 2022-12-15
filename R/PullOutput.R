# Script to list all scenario and pull, compile, and save output from SSMSE folders
# Created: 11/30/2022, Robert Wildermuth

library(r4ss)
library(tidyverse)

source("../SardineMSE/R/GetSumryOutput.R")

# Read in data and calculate metrics --------------------------------------

mseDir <- "J:/Desiree/Sardine/SardineScenarios"

addlOutputPath <- "J:/Desiree/Sardine/SardineScenarios/addlRuns"

scenarios <- c("constGrow2001OM_constGrow2005EM_RegRecHCR0",
               "constGrow2001OM_constGrow2005EM_RegRecHCR1",
               "constGrow2001OM_constGrow2005EM_RegRecHCR2",
               "constGrow2001OM_constGrow2005EM_RegRecHCR3",
               "constGrow2001OM_constGrow2005EM_RegRecHCR9",
               "constGrow2001OM_constGrow2005EM_RegRecHCR5",
               "constGrow2001OM_constGrow2005EM_RegRecHCR6",
               "constGrow2001OM_constGrow2005EM_RegRecHCR7",
               "constGrow2001OM_constGrow2005EM_RegRecHCR8",
  
               # "constGrow2001OM_constGrow2005EM_ARRecHCR0",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR1",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR2",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR3",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR9",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR5",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR6",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR7",
               # "constGrow2001OM_constGrow2005EM_ARRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR0",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR1",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR2",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR3",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR9",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR5",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR6",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR7",
               # "constGrow2001OM_constGrow2005EM_SSTRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR0",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR1",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR2",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR3",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR9",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR5",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR6",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR7",
               # "constGrow2001OM_constGrow2005EM_PDOclimRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR0",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR1",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR2",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR3",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR9",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR5",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR6",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR7",
               # "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_MICERecHCR0",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR1",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR2",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR3",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR9",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR5",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR6",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR7",
               # "constGrow2001OM_constGrow2005EM_MICERecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR0",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR1",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR2",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR3",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR9",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR5",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR6",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR7",
               # "constGrow2001OM_constGrow2005EM_invRegRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_invARRecHCR0",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR1",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR2",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR3",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR9",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR5",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR6",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR7",
               # "constGrow2001OM_constGrow2005EM_invARRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR0",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR1",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR2",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR3",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR9",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR5",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR6",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR7",
               # "constGrow2001OM_constGrow2005EM_invSSTRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR0",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR1",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR2",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR3",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR9",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR5",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR6",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR7",
               # "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR8",
               # 
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR0",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR1",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR2",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR3",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR9",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR5",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR6",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR7",
               # "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR8",
               
               "constGrow2001OM_constGrow2005EM_invMICERecHCR0",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR1",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR2",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR3",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR9",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR5",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR6",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR7",
               "constGrow2001OM_constGrow2005EM_invMICERecHCR8")

smryOutputList <- GetSumryOutput(dirSSMSE = mseDir, scenarios = scenarios,
                                 comps = TRUE, simData = TRUE)

smryAddlOutputList <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                     scenarios = scenarios,
                                     comps = TRUE, simData = TRUE)

# parse iteration, scenario, and model_run from composition and simDat tables
# find columns for directory parsing
dirsCol <- ncol(str_split(smryOutputList$ageComp$resDir[1], 
                          pattern = "/", simplify = TRUE))

smryOutputList$ageComp <- smryOutputList$ageComp %>%
                            mutate(model_run = str_split(resDir, pattern = "/", 
                                                         simplify = TRUE)[, dirsCol],
                                   iteration = as.integer(str_split(resDir, 
                                                                    pattern = "/", 
                                                                    simplify = TRUE)[, dirsCol-1]), 
                                   scenario = str_split(resDir, pattern = "/",
                                                        simplify = TRUE)[, dirsCol-2])

smryOutputList$lenComp <- smryOutputList$lenComp %>%
                            mutate(model_run = str_split(resDir, pattern = "/", 
                                                         simplify = TRUE)[, dirsCol],
                                   iteration = as.integer(str_split(resDir, 
                                                                    pattern = "/", 
                                                                    simplify = TRUE)[, dirsCol-1]), 
                                   scenario = str_split(resDir, pattern = "/",
                                                        simplify = TRUE)[, dirsCol-2])

smryOutputList$obsCPUE <- smryOutputList$obsCPUE %>%
                            mutate(model_run = str_split(resDir, pattern = "/", 
                                                         simplify = TRUE)[, dirsCol],
                                   iteration = as.integer(str_split(resDir, 
                                                                    pattern = "/", 
                                                                    simplify = TRUE)[, dirsCol-1]), 
                                   scenario = str_split(resDir, pattern = "/",
                                                        simplify = TRUE)[, dirsCol-2])

smryOutputList$obsCatch <- smryOutputList$obsCatch %>%
                            mutate(model_run = str_split(resDir, pattern = "/", 
                                                         simplify = TRUE)[, dirsCol],
                                   iteration = as.integer(str_split(resDir, 
                                                                    pattern = "/", 
                                                                    simplify = TRUE)[, dirsCol-1]), 
                                   scenario = str_split(resDir, pattern = "/",
                                                        simplify = TRUE)[, dirsCol-2])

# find columns for directory parsing
dirsCol <- ncol(str_split(smryAddlOutputList$ageComp$resDir[1], 
                          pattern = "/", simplify = TRUE))

smryAddlOutputList$ageComp <- smryAddlOutputList$ageComp %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

smryAddlOutputList$lenComp <- smryAddlOutputList$lenComp %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

smryAddlOutputList$obsCPUE <- smryAddlOutputList$obsCPUE %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

smryAddlOutputList$obsCatch <- smryAddlOutputList$obsCatch %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

# add original iteration number to additional iterations to be unique
maxIts <- max(smryOutputList$dqSmry$iteration)

smryAddlOutputList$dqSmry <- smryAddlOutputList$dqSmry %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList$sclSmry <- smryAddlOutputList$sclSmry %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList$tsSmry <- smryAddlOutputList$tsSmry %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList$ageComp <- smryAddlOutputList$ageComp %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList$lenComp <- smryAddlOutputList$lenComp %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList$obsCPUE <- smryAddlOutputList$obsCPUE %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList$obsCatch <- smryAddlOutputList$obsCatch %>% mutate(iteration = iteration + maxIts)

# combine output
smryOutputList$dqSmry <- smryOutputList$dqSmry %>% bind_rows(smryAddlOutputList$dqSmry)
smryOutputList$sclSmry <- smryOutputList$sclSmry %>% bind_rows(smryAddlOutputList$sclSmry)
smryOutputList$tsSmry <- smryOutputList$tsSmry %>% bind_rows(smryAddlOutputList$tsSmry)
smryOutputList$ageComp <- smryOutputList$ageComp %>% bind_rows(smryAddlOutputList$ageComp)
smryOutputList$lenComp <- smryOutputList$lenComp %>% bind_rows(smryAddlOutputList$lenComp)
smryOutputList$obsCPUE <- smryOutputList$obsCPUE %>% bind_rows(smryAddlOutputList$obsCPUE)
smryOutputList$obsCatch <- smryOutputList$obsCatch %>% bind_rows(smryAddlOutputList$obsCatch)

# Parse HCR and recruitment scenario
smryOutputList$dqSmry <- smryOutputList$dqSmry %>% 
                            mutate(HCR = sub(pattern = ".*Rec","", scenario),
                                   recScen = sub(pattern = "HCR.*","", scenario)) %>%
                            mutate(recScen = sub(pattern = ".*EM_","", recScen))
smryOutputList$sclSmry <- smryOutputList$sclSmry %>% 
  mutate(HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))
smryOutputList$tsSmry <- smryOutputList$tsSmry %>% 
  mutate(HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))
smryOutputList$ageComp <- smryOutputList$ageComp %>% 
  mutate(HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))
smryOutputList$lenComp <- smryOutputList$lenComp %>% 
  mutate(HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))
smryOutputList$obsCPUE <- smryOutputList$obsCPUE %>% 
  mutate(HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))
smryOutputList$obsCatch <- smryOutputList$obsCatch %>% 
  mutate(HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))

saveRDS(smryOutputList, 
        # file = file.path(mseDir, "serverRegARPDOcyclPDOclimSSTMICE_allHCRs_addlResults.RDS"))
        file = file.path(mseDir, "serverRegARtest_allHCRs_addlResults.RDS"))
