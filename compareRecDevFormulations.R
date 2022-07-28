# Compare methods for deriving recruitment deviations

library(tidyverse)
library(r4ss)

# Read in the cyclic PDO expectation and current method of generating devs
cyclPDOdevs <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")

# Proportional environmental attribution
PDOCoeff <- 0.7815
cyclPDOdevs <- cyclPDOdevs %>% mutate(addlError = rnorm(nrow(cyclPDOdevs),0, 1.25),
                                      recDevProp = 0.44*(PDOCoeff*expPDO) + 0.56*addlError)
                                      
                                      
# Variance around mean environmental effect
cyclPDOdevs <- cyclPDOdevs %>% mutate(recDevMean = PDOCoeff*expPDO + addlError)

# Add additional error over environment (first method tried)
cyclPDOdevs <- cyclPDOdevs %>% mutate(recDevAddl = recDevPDO * 0.7 + (0.3 * addlError))

# Read in historical devs
research1981 <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment")
hist1981Rec <- research1981$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
  rename("Hist1981" = "dev")

research2001 <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowthMidSteepNewSelex_OM")
hist2001Rec <- research2001$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
  rename("Hist2001" = "dev")

cyclPDOdevs <- cyclPDOdevs %>% full_join(y = hist1981Rec, by = c("Year" = "Yr")) %>%
                  full_join(y = hist2001Rec, by = c("Year" = "Yr")) %>%
                  pivot_longer(cols = c(expPDO, recDevPDO, recDevProp,
                                        recDevMean, recDevAddl, Hist1981, Hist2001),
                               names_to = "Method", values_to = "recDev")

cyclPDOdevs %>% filter(Year < 2070) %>%
  ggplot(aes(x = Year, y = recDev, color = Method)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  theme_minimal()

cyclPDOdevs %>% filter(Year < 2070) %>%
  ggplot(aes(x = Year, y = recDev, color = Method)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  theme_minimal() +
  facet_wrap(.~Method)

cyclPDOdevs %>% group_by(Method) %>% summarize(devMean = mean(recDev, na.rm = TRUE),
                                               devSD = sd(recDev, na.rm = TRUE))

# Do same for SST devs ----------------------------------------------------

# Read in the cyclic PDO expectation and current method of generating devs
SSTdevs <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevSST2070.csv")

# Proportional environmental attribution
SSTCoeff <- 1.280965e+00
SSTdevs <- SSTdevs %>% mutate(addlError = rnorm(nrow(SSTdevs),0, 1.25),
                              recDevProp = 0.55*((SSTCoeff*gfdlSSTadj)-20.27469) + 0.45*addlError)


# Variance around mean environmental effect
SSTdevs <- SSTdevs %>% mutate(recDevMean = ((SSTCoeff*gfdlSSTadj)-20.27469) + addlError)

# Add additional error over environment (first method tried)
SSTdevs <- SSTdevs %>% mutate(recDevAddl = recDevSST_GFDL * 0.7 + (0.3 * addlError))

SSTdevs <- SSTdevs %>% full_join(y = hist1981Rec, by = c("year" = "Yr")) %>%
  full_join(y = hist2001Rec, by = c("year" = "Yr")) %>%
  pivot_longer(cols = c(gfdlSSTadj, recDevSST_GFDL, recDevSST_GFDLbc, recDevProp,
                        recDevMean, recDevAddl, Hist1981, Hist2001),
               names_to = "Method", values_to = "recDev")

SSTdevs %>% filter(year < 2070, Method != "gfdlSSTadj") %>%
  ggplot(aes(x = year, y = recDev, color = Method)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  theme_minimal()

SSTdevs %>% filter(year < 2070, Method != "gfdlSSTadj") %>%
  ggplot(aes(x = year, y = recDev, color = Method)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 2019.5, color = "grey") +
  theme_minimal() +
  facet_wrap(.~Method)

SSTdevs %>% group_by(Method) %>% summarize(devMean = mean(recDev, na.rm = TRUE),
                                           devSD = sd(recDev, na.rm = TRUE))

