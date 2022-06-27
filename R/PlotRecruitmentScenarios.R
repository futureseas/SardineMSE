# Plot of recruitment scenario deviations

library(tidyverse)
library(r4ss)

# Historical recruitment from operating model
om2001 <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001",
                    verbose = FALSE, printstats = FALSE)
omHistRec <- om2001$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
                mutate(scenario = "Historical")

# Random and autocorrelated recruitment projections
smryOutputList <- readRDS("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/serverRandRec_ARRec_allHCRs_results.RDS")
omRecDevs <- smryOutputList$tsSmry %>% filter(Seas == 1, 
                                              model_run == "start2001_OM",
                                              iteration == 5,
                                              grepl("HCR0", scenario, fixed = TRUE)) %>%
                rename("Yr" = "year", "dev" = "rec_dev") %>%
                select(Yr, dev, scenario) %>%
                mutate(scenario = sub("constGrow2001OM_constGrow2005EM_", "", scenario),
                       scenario = sub("HCR0", "", scenario))
# randRec <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/start2001_OM_RandRecHCR0",
#                     verbose = FALSE, printstats = FALSE)
# randRec <- randRec$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
#               mutate(scenario = "Random")
# 
# # Random recruitment with autocorrelation projection
# arRec <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/start2001_OM_ARRecHCR0",
#                      verbose = FALSE, printstats = FALSE)
# arRec <- arRec$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
#             mutate(scenario = "AR1")

# deviations from cyclic PDO relationship
recPDOnoclim <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")
recPDOnoclim <- recPDOnoclim %>% select(Year, recDevPDO) %>%
                  rename("Yr" = "Year",
                         "dev" = "recDevPDO") %>%
                  mutate(scenario = "PDOnoclim")

recSST <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevSST2070.csv")
recSST <- recSST %>% select(year, recDevSST_IPSL, recDevSST_HAD, recDevSST_GFDL) %>%
            rename("Yr" = "year") %>%
            # mutate(scenario = "SST") %>%
            pivot_longer(cols = c(recDevSST_IPSL, recDevSST_HAD, recDevSST_GFDL),
                         names_to = "scenario",
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

# recDevDat <- rbind(omHistRec, randRec, arRec)
# recDevDat <- recDevDat %>% select(Yr, dev, scenario)
# recDevDat <- rbind(recDevDat, recPDOnoclim, recSST, ssbrecsMICE, climPDO)
recDevDat <- rbind(omRecDevs, recPDOnoclim, recSST, ssbrecsMICE, climPDO)

recDevDat %>% filter(scenario %in% c("RandRec", "ARRec", "PDOnoclim", 
                                     "recDevSST_GFDL", "GFDL_ensemble", "PDO_GFDL"),
                     Yr < 2070, Yr > 2000) %>%
  mutate(scenario = fct_relevel(scenario, "RandRec", "ARRec", "PDOnoclim", 
                                "PDO_GFDL", "recDevSST_GFDL", "GFDL_ensemble")) %>%
  ggplot(aes(x = Yr, y = dev, color = scenario)) + 
  geom_line() + 
  facet_wrap(vars(scenario)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 2019.5, color = "darkgrey") +
  labs(x = "Year", y = "Recruitment Deviation")

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