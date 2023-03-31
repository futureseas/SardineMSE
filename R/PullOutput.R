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
  
               "constGrow2001OM_constGrow2005EM_ARRecHCR0",
               "constGrow2001OM_constGrow2005EM_ARRecHCR1",
               "constGrow2001OM_constGrow2005EM_ARRecHCR2",
               "constGrow2001OM_constGrow2005EM_ARRecHCR3",
               "constGrow2001OM_constGrow2005EM_ARRecHCR9",
               "constGrow2001OM_constGrow2005EM_ARRecHCR5",
               "constGrow2001OM_constGrow2005EM_ARRecHCR6",
               "constGrow2001OM_constGrow2005EM_ARRecHCR7",
               "constGrow2001OM_constGrow2005EM_ARRecHCR8",

               "constGrow2001OM_constGrow2005EM_SSTRecHCR0",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR1",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR2",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR3",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR9",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR5",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR6",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR7",
               "constGrow2001OM_constGrow2005EM_SSTRecHCR8",

               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR0",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR1",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR2",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR3",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR9",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR5",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR6",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR7",
               "constGrow2001OM_constGrow2005EM_PDOclimRecHCR8",

               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR0",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR1",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR2",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR3",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR9",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR5",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR6",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR7",
               "constGrow2001OM_constGrow2005EM_PDOcyclRecHCR8",

               "constGrow2001OM_constGrow2005EM_MICERecHCR0",
               "constGrow2001OM_constGrow2005EM_MICERecHCR1",
               "constGrow2001OM_constGrow2005EM_MICERecHCR2",
               "constGrow2001OM_constGrow2005EM_MICERecHCR3",
               "constGrow2001OM_constGrow2005EM_MICERecHCR9",
               "constGrow2001OM_constGrow2005EM_MICERecHCR5",
               "constGrow2001OM_constGrow2005EM_MICERecHCR6",
               "constGrow2001OM_constGrow2005EM_MICERecHCR7",
               "constGrow2001OM_constGrow2005EM_MICERecHCR8")

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
               
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR0",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR1",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR2",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR3",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR9",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR5",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR6",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR7",
               # "constGrow2001OM_constGrow2005EM_invMICERecHCR8")

smryOutputList <- GetSumryOutput(dirSSMSE = mseDir, scenarios = scenarios,
                                 comps = TRUE, simData = TRUE)
saveRDS(smryOutputList, 
        file = file.path(mseDir, "server100its_allHCRS.RDS"))
smryOutputList <- readRDS(file = file.path(mseDir, "server100its_allHCRS.RDS"))
# have to split for parallel processing
smryAddlOutputList1 <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                     scenarios = scenarios[1:9],
                                     comps = TRUE, simData = TRUE)
saveRDS(smryAddlOutputList1, 
        file = file.path(mseDir, "server400itsRegRec_allHCRs.RDS"))
smryAddlOutputList1 <- readRDS(file = file.path(mseDir, "server400itsRegRec_allHCRs.RDS"))
smryAddlOutputList2 <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                      scenarios = scenarios[10:18],
                                      comps = TRUE, simData = TRUE)
saveRDS(smryAddlOutputList2, 
        file = file.path(mseDir, "server400itsARRec_allHCRs.RDS"))
smryAddlOutputList2 <- readRDS(file = file.path(mseDir, "server400itsARRec_allHCRs.RDS"))
smryAddlOutputList3 <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                     scenarios = scenarios[19:27],
                                     comps = TRUE, simData = TRUE)
saveRDS(smryAddlOutputList3, 
        file = file.path(mseDir, "server400itsSSTRec_allHCRs.RDS"))
smryAddlOutputList3 <- readRDS(file = file.path(mseDir, "server400itsSSTRec_allHCRs.RDS"))
smryAddlOutputList4 <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                      scenarios = scenarios[28:36],
                                      comps = TRUE, simData = TRUE)
saveRDS(smryAddlOutputList4, 
        file = file.path(mseDir, "server400itsPDOclimRec_allHCRs.RDS"))
smryAddlOutputList4 <- readRDS(file = file.path(mseDir, "server400itsPDOclimRec_allHCRs.RDS"))
smryAddlOutputList5 <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                      scenarios = scenarios[37:45],
                                      comps = TRUE, simData = TRUE)
saveRDS(smryAddlOutputList5, 
        file = file.path(mseDir, "server400itsPDOcyclRec_allHCRs.RDS"))
smryAddlOutputList5 <- readRDS(file = file.path(mseDir, "server400itsPDOcyclRec_allHCRs.RDS"))
smryAddlOutputList6 <- GetSumryOutput(dirSSMSE = addlOutputPath, 
                                      scenarios = scenarios[46:54],
                                      comps = TRUE, simData = TRUE)
saveRDS(smryAddlOutputList6, 
        file = file.path(mseDir, "server400itsMICERec_allHCRs.RDS"))
smryAddlOutputList6 <- readRDS(file = file.path(mseDir, "server400itsMICERec_allHCRs.RDS"))

# combine output
smryAddlOutputList1$dqSmry <- smryAddlOutputList1$dqSmry %>% bind_rows(smryAddlOutputList2$dqSmry, smryAddlOutputList3$dqSmry, smryAddlOutputList4$dqSmry, smryAddlOutputList5$dqSmry, smryAddlOutputList6$dqSmry)
smryAddlOutputList1$sclSmry <- smryAddlOutputList1$sclSmry %>% bind_rows(smryAddlOutputList2$sclSmry, smryAddlOutputList3$sclSmry, smryAddlOutputList4$sclSmry, smryAddlOutputList5$sclSmry, smryAddlOutputList6$sclSmry)
smryAddlOutputList1$tsSmry <- smryAddlOutputList1$tsSmry %>% bind_rows(smryAddlOutputList2$tsSmry, smryAddlOutputList3$tsSmry, smryAddlOutputList4$tsSmry, smryAddlOutputList5$tsSmry, smryAddlOutputList6$tsSmry)
smryAddlOutputList1$ageComp <- smryAddlOutputList1$ageComp %>% bind_rows(smryAddlOutputList2$ageComp, smryAddlOutputList3$ageComp,smryAddlOutputList4$ageComp, smryAddlOutputList5$ageComp,smryAddlOutputList6$ageComp)
smryAddlOutputList1$lenComp <- smryAddlOutputList1$lenComp %>% bind_rows(smryAddlOutputList2$lenComp, smryAddlOutputList3$lenComp, smryAddlOutputList4$lenComp, smryAddlOutputList5$lenComp, smryAddlOutputList6$lenComp)
smryAddlOutputList1$obsCPUE <- smryAddlOutputList1$obsCPUE %>% bind_rows(smryAddlOutputList2$obsCPUE, smryAddlOutputList3$obsCPUE, smryAddlOutputList4$obsCPUE, smryAddlOutputList5$obsCPUE, smryAddlOutputList6$obsCPUE)
smryAddlOutputList1$obsCatch <- smryAddlOutputList1$obsCatch %>% bind_rows(smryAddlOutputList2$obsCatch, smryAddlOutputList3$obsCatch, smryAddlOutputList4$obsCatch, smryAddlOutputList5$obsCatch, smryAddlOutputList6$obsCatch)


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
dirsCol <- ncol(str_split(smryAddlOutputList1$ageComp$resDir[1], 
                          pattern = "/", simplify = TRUE))

smryAddlOutputList1$ageComp <- smryAddlOutputList1$ageComp %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

smryAddlOutputList1$lenComp <- smryAddlOutputList1$lenComp %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

smryAddlOutputList1$obsCPUE <- smryAddlOutputList1$obsCPUE %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

smryAddlOutputList1$obsCatch <- smryAddlOutputList1$obsCatch %>%
  mutate(model_run = str_split(resDir, pattern = "/", 
                               simplify = TRUE)[, dirsCol],
         iteration = as.integer(str_split(resDir, 
                                          pattern = "/", 
                                          simplify = TRUE)[, dirsCol-1]), 
         scenario = str_split(resDir, pattern = "/",
                              simplify = TRUE)[, dirsCol-2])

# add original iteration number to additional iterations to be unique
maxIts <- max(smryOutputList$dqSmry$iteration)

smryAddlOutputList1$dqSmry <- smryAddlOutputList1$dqSmry %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList1$sclSmry <- smryAddlOutputList1$sclSmry %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList1$tsSmry <- smryAddlOutputList1$tsSmry %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList1$ageComp <- smryAddlOutputList1$ageComp %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList1$lenComp <- smryAddlOutputList1$lenComp %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList1$obsCPUE <- smryAddlOutputList1$obsCPUE %>% mutate(iteration = iteration + maxIts)
smryAddlOutputList1$obsCatch <- smryAddlOutputList1$obsCatch %>% mutate(iteration = iteration + maxIts)

# combine output
smryOutputList$dqSmry <- smryOutputList$dqSmry %>% bind_rows(smryAddlOutputList1$dqSmry)
smryOutputList$sclSmry <- smryOutputList$sclSmry %>% bind_rows(smryAddlOutputList1$sclSmry)
smryOutputList$tsSmry <- smryOutputList$tsSmry %>% bind_rows(smryAddlOutputList1$tsSmry)
smryOutputList$ageComp <- smryOutputList$ageComp %>% bind_rows(smryAddlOutputList1$ageComp)
smryOutputList$lenComp <- smryOutputList$lenComp %>% bind_rows(smryAddlOutputList1$lenComp)
smryOutputList$obsCPUE <- smryOutputList$obsCPUE %>% bind_rows(smryAddlOutputList1$obsCPUE)
smryOutputList$obsCatch <- smryOutputList$obsCatch %>% bind_rows(smryAddlOutputList1$obsCatch)

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
        file = file.path(mseDir, "serverRegARPDOcyclPDOclimSSTMICE_allHCRs_addlResults.RDS"))
        # file = file.path(mseDir, "serverRegARtest_allHCRs_addlResults.RDS"))
