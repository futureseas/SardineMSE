library(r4ss)

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