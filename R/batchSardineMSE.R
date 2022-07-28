# Batch source code for multiple SSMSE runs on the server

dir <- "J:/Desiree/Sardine/SardineMSE/R"
mseOutputPath <- "J:/Desiree/Sardine/SardineScenarios"

logFile <- paste0(mseOutputPath, "/SardineMSElog_", Sys.Date(), ".log")

sink(file = file(logFile), append = TRUE)

# source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_RandRec_MSE.R"))
#source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_ARRec_MSE.R"))
source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_SSTRec_MSE.R"))
source(file.path(dir, "constGrowth2001OM_constGrowth2005EM_PDOcyclRec_MSE.R"))

sink()
