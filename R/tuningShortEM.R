library(r4ss)
library(tidyverse)

# Looking at effect of initF estimation among fleets/surveys
origst2005EM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/EMconvergCheck/initF_adjustments/EM_st2005_original",
                         covar = FALSE)
SS_plots(origst2005EM)

S1and2PWNEM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/EMconvergCheck/initF_adjustments/EM_st2005_S1and2PNW",
                          covar = FALSE)
SS_plots(S1and2PWNEM)

S1MexCal1EM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/EMconvergCheck/initF_adjustments/EM_st2005_S1MexCal1",
                          covar = FALSE)
SS_plots(S1MexCal1EM)

S1PNWEM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/EMconvergCheck/initF_adjustments/EM_st2005_S1PNW",
                          covar = FALSE)
SS_plots(S1PNWEM)

S2PNWEM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/EMconvergCheck/initF_adjustments/EM_st2005_S2PNW",
                          covar = FALSE)
SS_plots(S2PNWEM)

allFleetsEM <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/EMconvergCheck/initF_adjustments/EM_st2005_allFleets",
                          covar = FALSE)
SS_plots(allFleetsEM)

# looking at mean of sample sizes for composition data among fleets/surveys
dat1981 <- SS_readdat("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start1981/OM/OM_K/dat.ss")
dat1981$lencomp %>% filter(FltSvy %in% 1:4, Yr > 2000) %>% group_by(FltSvy) %>% 
  summarize(meanNsamp = mean(Nsamp),
            maxNsamp = max(Nsamp),
            minNsamp = min(Nsamp))

dat1981$agecomp %>% filter(FltSvy %in% 1:4) %>% #, Yr > 2000) %>% 
  group_by(FltSvy) %>% 
  summarize(meanNsamp = mean(Nsamp),
            maxNsamp = max(Nsamp),
            minNsamp = min(Nsamp))
