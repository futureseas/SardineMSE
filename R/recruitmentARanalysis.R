# Analysis of autocorrelation in the recruitment deviation time series
# Created: 7/5/2022, Robert Wildermuth

library(r4ss)
library(tidyverse)
researchOutput <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment")

longRecDevs <- researchOutput$recruit %>% filter(era == "Main")

resAR <- acf(x = longRecDevs$dev)
resAR$acf
sd(longRecDevs$dev)
longRecDevs %>% ggplot(aes(x = Yr, y = dev)) +
  geom_line()

# 60-cycle PDO rec devs
recPDOnoclim <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")
noClimPDO <- acf(x = recPDOnoclim$recDevPDO)
sd(recPDOnoclim$recDevPDO)

# ROMS GDFL PDO rec devs
recPDOclim <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOclim2101.csv")
recPDOclim <- recPDOclim %>% filter(year > 2019)
climPDO <- acf(x = recPDOclim$recDev_GFDL)
sd(recPDOclim$recDev_GFDL)

# SST rec devs
recSST <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevSST2070.csv")
recSST <- recSST %>% filter(year > 2019)
climSST <- acf(x = recSST$recDevSST_GFDL)
sd(recSST$recDevSST_GFDL)

# MICE rec devs
ssbrecsMICE <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevMICE2100.csv")
ssbrecsMICE <- ssbrecsMICE %>% filter(Year > 2019, GCM == "GFDL")
miceAR <- acf(x = ssbrecsMICE$ensembleRecDevs)
miceAR$acf
sd(ssbrecsMICE$ensembleRecDevs)
