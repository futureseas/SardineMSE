# Plot of recruitment scenario deviations

library(tidyverse)
library(r4ss)

# Historical recruitment from operating model
om2001 <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowthMidSteepNewSelex_OM",
                    verbose = FALSE, printstats = FALSE)
omHistRec <- om2001$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
                mutate(scenario = "Historical")

# Historical recruitment from 1981 model
research1981 <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment",
                          verbose = FALSE, printstats = FALSE)
resHistRec <- research1981$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
                mutate(scenario = "Hist1981")

# Random and autocorrelated recruitment projections

# smryOutputList <- readRDS("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/serverRandRec_ARRec_allHCRs_results.RDS")
# omRecDevs <- smryOutputList$tsSmry %>% filter(Seas == 1, 
#                                               model_run == "start2001_OM",
#                                               iteration == 5,
#                                               grepl("HCR0", scenario, fixed = TRUE)) %>%
#                 rename("Yr" = "year", "dev" = "rec_dev") %>%
#                 select(Yr, dev, scenario) %>%
#                 mutate(scenario = sub("constGrow2001OM_constGrow2005EM_", "", scenario),
#                        scenario = sub("HCR0", "", scenario))
randRec <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_constGrow2005EM_RandRecHCR0/1/constGrowthMidSteepNewSelex_OM_OM",
                    verbose = FALSE, printstats = FALSE)
randRec <- randRec$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
              mutate(scenario = "RandRec")

# Random recruitment with autocorrelation projection
arRec <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_constGrow2005EM_ARRecHCR0/1/constGrowthMidSteepNewSelex_OM_OM",
                     verbose = FALSE, printstats = FALSE)
arRec <- arRec$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
            mutate(scenario = "ARRec")


# Random recruitment with regime decrease and increase
regDecRec <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_constGrow2005EM_RegRecHCR0/1/constGrowthMidSteepNewSelex_OM_OM",
                   verbose = FALSE, printstats = FALSE)
regDecRec <- regDecRec$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
  mutate(scenario = "RegDecRec")

regIncRec <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_constGrow2005EM_RegRecHCR0/4/constGrowthMidSteepNewSelex_OM_OM",
                       verbose = FALSE, printstats = FALSE)
regIncRec <- regIncRec$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
  mutate(scenario = "RegIncRec")

# deviations from cyclic PDO relationship
recPDOnoclim <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")
recPDOnoclim <- recPDOnoclim %>% select(Year, recDevPDO) %>%
                  rename("Yr" = "Year",
                         "dev" = "recDevPDO") %>%
                  mutate(scenario = "PDOnoclim")

# Add additional error over environment, different among iterations but same across HCRs
recPDOnoclim <- recPDOnoclim %>% mutate(addlError = rnorm(nrow(recPDOnoclim),0, 1.25),
                            # pseudo-R^2 of PDO fit was 0.44 in Zwolinski & Demer 2019
                            valueNew = dev * 0.44 + (0.56 * addlError),
                            devSD = sd(valueNew))
# do scale correction
recPDOnoclim <- recPDOnoclim %>% mutate(valueNewScaled = valueNew * (1.25/devSD)) %>%
                    pivot_longer(cols = c(dev, valueNew, valueNewScaled),
                                 names_to = "scaleStep",
                                 values_to = "dev")

recSST <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevSST2070.csv")
recSST <- recSST %>% select(year, recDevSST_IPSL, recDevSST_HAD, recDevSST_GFDL) %>%
            rename("Yr" = "year") %>%
            # mutate(scenario = "SST") %>%
            pivot_longer(cols = c(recDevSST_IPSL, recDevSST_HAD, recDevSST_GFDL),
                         names_to = "scenario",
                         values_to = "dev")

# Add additional error over environment, different among iterations but same across HCRs
recSST <- recSST %>% mutate(addlError = rnorm(nrow(recSST),0, 1.25),
                          # R^2 of annual SST model was 0.55 (PFMC 2013, Appendix E, pg 49) 
                          valueNew = dev * 0.55 + (0.45 * addlError),
                          devSD = sd(valueNew))
# do scale correction
recSST <- recSST %>% mutate(valueNewScaled = valueNew * (1.25/devSD)) %>%
            pivot_longer(cols = c(dev, valueNew, valueNewScaled),
                         names_to = "scaleStep",
                         values_to = "dev")

ssbrecsMICE <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevMICE2100.csv")
ssbrecsMICE <- ssbrecsMICE %>% rename("Yr" = "Year") %>%
                  select(Yr, GCM, ensembleRecDevs, elsRecDevs) %>%
                  pivot_longer(cols = c(ensembleRecDevs, elsRecDevs),
                               names_to = c("ensemble", "scenario"), 
                               names_sep = "RecDevs",
                               values_to = "dev") %>%
                  mutate(scenario = paste(GCM, ensemble, sep = "_")) %>%
                  select(Yr, dev, scenario)

climPDO <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOclim2101.csv")

climPDO <- climPDO %>% rename("Yr" = "year") %>%
  select(Yr, recDev_GFDL, recDev_IPSL, recDev_HAD) %>%
  pivot_longer(cols = c(recDev_GFDL, recDev_IPSL, recDev_HAD),
               names_to = c("indicator", "scenario"), 
               names_sep = "_",
               values_to = "dev") %>%
  mutate(scenario = paste("PDO", scenario, sep = "_")) %>%
  select(Yr, dev, scenario)


recDevDat <- rbind(omHistRec, resHistRec, randRec, arRec, regDecRec, regIncRec)
recDevDat <- recDevDat %>% select(Yr, dev, scenario) %>% 
                mutate(scaleStep = "ID")
envtDev <- rbind(recPDOnoclim, recSST)#, ssbrecsMICE, climPDO)
envtDev <- envtDev %>% select(Yr, dev, scenario, scaleStep)
recDevDat <- rbind(recDevDat, envtDev)
# recDevDat <- rbind(omRecDevs, recPDOnoclim, recSST, ssbrecsMICE, climPDO)

resHistRec$Yr <- as.numeric(resHistRec$Yr)

incRegMean <- regIncRec %>% filter(Yr > 2044) %>% summarize(meanDev = mean(dev))

df <- data.frame(x1 = c(1981, 2001, 2020, 2045), 
                 x2 = c(2000, 2019, 2044, 2068), 
                 y1 = c(mean(resHistRec[resHistRec$Yr < 2001, "dev"], na.rm = TRUE),
                        mean(resHistRec[resHistRec$Yr > 2000, "dev"], na.rm = TRUE),
                        0, incRegMean$meanDev), 
                 y2 = c(mean(resHistRec[resHistRec$Yr < 2001, "dev"], na.rm = TRUE),
                        mean(resHistRec[resHistRec$Yr > 2000, "dev"], na.rm = TRUE),
                        0, incRegMean$meanDev),
                 scenario = c("Hist1981", "Hist1981",
                              "RegIncRec", "RegIncRec"))

recDevDat %>% filter(scenario %in% c("RandRec", "ARRec", "PDOnoclim", "Hist1981", "Historical", "RegDecRec", "RegIncRec",
                                     "recDevSST_GFDL"),#, "GFDL_ensemble", "PDO_GFDL"),
                     Yr < 2070, Yr > 1981) %>%
  # mutate(scenario = fct_relevel(scenario, "RandRec", "ARRec", "PDOnoclim", 
  #                               "PDO_GFDL", "recDevSST_GFDL", "GFDL_ensemble")) %>%
  ggplot(aes(x = Yr, y = dev, color = scaleStep)) + 
  geom_line() + 
  facet_wrap(vars(scenario)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 2019.5, color = "darkgrey") +
  labs(x = "Year", y = "Recruitment Deviation") +
  geom_segment(data = df,
               mapping = aes(x = x1, y = y1, xend = x2, yend = y2, colour = "Regime"))

recDevDat %>% ggplot(aes(x = dev, color = scenario)) + 
  geom_histogram() +
  facet_wrap(vars(scenario)) +
  theme_classic() +
  geom_vline(xintercept = 0, color = "red")

# add example variance expansion on environmental scenarios 
envtScens <- recDevDat %>% filter(!scenario %in% c('AR1', 'Historical', 'Random')) 
envtScens <- envtScens %>%
              mutate(addlError = rnorm(nrow(envtScens),0,1.25)) %>%
              mutate(devExpand = dev * 0.7 + (0.3 * addlError),
                     expVar = paste(scenario, "expVar", sep = "_"))
recDevDat$expVar <- recDevDat$scenario
envtScens <- envtScens %>% select(Yr, devExpand, scenario, expVar) %>%
                rename("dev" = "devExpand")
recDevDat <- rbind(recDevDat, envtScens)

recDevDat %>% filter(scenario %in% c('AR1', 'Historical', 'Random', 
                                     'recDevSST_GFDL', 'PDOnoclim', 
                                     'PDO_GFDL', 'GFDL_ensemble')) %>%
  ggplot(aes(x = Yr, y = dev, color = expVar)) + 
  geom_line() + 
  facet_wrap(vars(scenario)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 2019.5, color = "darkgrey")

# fit linear models to see if trends in environmental projections
test5 <- climPDO %>% filter(scenario == "PDO_GFDL",
                            Yr > 2019)
fitPDO_GFDL <- lm(dev ~ 0 + Yr, data = test5)
summary(fitPDO_GFDL)

test6 <- climPDO %>% filter(scenario == "PDO_HAD",
                            Yr > 2019)
fitPDO_HAD <- lm(dev ~ 0 + Yr, data = test6)
summary(fitPDO_HAD)

test7 <- climPDO %>% filter(scenario == "PDO_IPSL",
                            Yr > 2019)
fitPDO_IPSL <- lm(dev ~ 0 + Yr, data = test7)
summary(fitPDO_IPSL)

test8 <- recSST %>% filter(scenario == "recDevSST_GFDL",
                            Yr > 2019)
fitSST_GFDL <- lm(dev ~ 0 + Yr, data = test8)
summary(fitSST_GFDL)

test9 <- ssbrecsMICE %>% filter(scenario == "GFDL_ensemble",
                                Yr > 2019)
fitMICE <- lm(dev ~ 0 + Yr, data = test9)
summary(fitMICE)