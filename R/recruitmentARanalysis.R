library(r4ss)
library(tidyverse)
researchOutput <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment")

longRecDevs <- researchOutput$recruit %>% filter(era == "Main")

resAR <- acf(x = longRecDevs$dev)

