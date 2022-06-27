# Code to modify Stock Synthesis models for use as OM or EM in SSMSE
# Created: 5/17/2022; Robert Wildermuth

library(r4ss)

# example code from SSMSE documentation
# develop_OMs will save a model called "cod_SR_BH_steep_1" in the out_dir specified
# develop_OMs(OM_name = "cod", out_dir = run_SSMSE_dir, par_name = "SR_BH_steep",
#             par_vals = 1, refit_OMs = FALSE, hess = FALSE)

# 5/17/2022: adjusted start2001/constGrowth model by hand to have higher steepness  
# and re-fit models to have better starting values for OM simulation

# Code to check why model wasn't fitting appropriately
omOut <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_selfTestSteep0dot6FixRec_RandRecHCR2/1/constGrowthSteepness0dot6_OM_OM")
fixedOut <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_selfTestSteep0dot6FixRec_RandRecHCR2/1/constGrowFixRec_EM_2032")
compFixed <- SSsummarize(list(OM = omOut, EM2032 = fixedOut))
compFixed$pars$relErr <- round((compFixed$pars$EM2032 - compFixed$pars$OM)/compFixed$pars$OM, digits = 3)
SSplotComparisons(compFixed)
compFixed$pars
omDat <- SS_readdat("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_selfTestSteep0dot6FixRec_RandRecHCR2/1/constGrowthSteepness0dot6_OM_OM/data.ss_new", section = 2)
emDat <- SS_readdat("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_selfTestSteep0dot6FixRec_RandRecHCR2/1/constGrowFixRec_EM_2032/data.ss_new")
omDat$CPUE$index <- abs(omDat$CPUE$index)
compDat <- omDat$CPUE %>% left_join(y = emDat$CPUE, by = c("year", "seas", "index")) %>%
  filter(index == 4) %>% pivot_longer(cols = c(obs.x, obs.y), names_to = "model", values_to = "CPUE")
compDat %>% ggplot(aes(x = year, y = CPUE)) + geom_line(aes(group = model))

# need to modify CPUE CV and composition sample size to be consistent for simulating data to fit to with EM
# omDat <- SS_readdat("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowthSteepness0dot6_OM/data.ss")
omDat <- SS_readdat("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowth/data.ss")
omDat$CPUE <- omDat$CPUE %>% mutate(se_log = case_when(index == 4 ~ 0.25,
                                                       TRUE ~ se_log))
omDat$lencomp <- omDat$lencomp %>% mutate(Nsamp = 100)
omDat$agecomp <- omDat$agecomp %>% mutate(Nsamp = 100)
SS_writedat(datlist = omDat,
            outfile = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowth/data.ss",
            # outfile = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowthSteepness0dot6_OM/data.ss",
            overwrite = TRUE)
