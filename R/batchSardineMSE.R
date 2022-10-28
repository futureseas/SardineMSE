# Batch source code for multiple SSMSE runs on the server

dir <- "J:/Desiree/Sardine/SardineMSE/R"
mseOutputPath <- "J:/Desiree/Sardine/SardineScenarios"

logFile <- paste0(mseOutputPath, "/SardineMSElog_", Sys.Date(), ".log")

sink(file = file(logFile), append = TRUE)

# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_RandRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_ARRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_MeanRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_RegRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_SSTRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_PDOcyclRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_PDOccRec_MSE.R"))
# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_MICERec_MSE.R"))

# run inverse rec dev scenarios
source(file.path(dir, "RunInverseScenarios.R"))

RunInverseScenarios(scenVec = c("constGrow2001OM_constGrow2005EM_invMICERecHCR0",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR1",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR2",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR3",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR5",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR6",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR7",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR8",
                                "constGrow2001OM_constGrow2005EM_invMICERecHCR9"),
                    mseOutputPath = mseOutputPath, seedNum = 729)

RunInverseScenarios(scenVec = c("constGrow2001OM_constGrow2005EM_invARRecHCR0",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR1",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR2",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR3",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR5",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR6",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR7",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR8",
                                "constGrow2001OM_constGrow2005EM_invARRecHCR9"),
                    mseOutputPath = mseOutputPath, seedNum = 729)

RunInverseScenarios(scenVec = c("constGrow2001OM_constGrow2005EM_invRegRecHCR0",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR1",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR2",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR3",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR5",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR6",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR7",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR8",
                                "constGrow2001OM_constGrow2005EM_invRegRecHCR9"),
                    mseOutputPath = mseOutputPath, seedNum = 729)

RunInverseScenarios(scenVec = c("constGrow2001OM_constGrow2005EM_invSSTRecHCR0",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR1",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR2",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR3",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR5",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR6",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR7",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR8",
                                "constGrow2001OM_constGrow2005EM_invSSTRecHCR9"),
                    mseOutputPath = mseOutputPath, seedNum = 729)

RunInverseScenarios(scenVec = c("constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR0",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR1",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR2",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR3",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR5",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR6",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR7",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR8",
                                "constGrow2001OM_constGrow2005EM_invPDOcyclRecHCR9"),
                    mseOutputPath = mseOutputPath, seedNum = 729)

RunInverseScenarios(scenVec = c("constGrow2001OM_constGrow2005EM_invPDOclimRecHCR0",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR1",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR2",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR3",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR5",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR6",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR7",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR8",
                                "constGrow2001OM_constGrow2005EM_invPDOclimRecHCR9"),
                    mseOutputPath = mseOutputPath, seedNum = 729)

sink()

rmarkdown::render(paste(dir,"SummarizeSardineMSE.Rmd", sep = "/"))