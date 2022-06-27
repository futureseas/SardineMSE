# compare performance metrics among scenarios

library(tidyverse)

# directory for MSE output
mseOutputPath <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/margCompsOMfixedSelexEM"
load(file = file.path(mseOutputPath, "resRandRecHCR0.RData"))
metricsHCR0 <- metrics %>% mutate(mngtStrategy = "NoCatch")

load(file = file.path(mseOutputPath, "resRandRecHCR1.RData"))
metricsHCR1 <- metrics %>% mutate(mngtStrategy = "Current")

load(file = file.path(mseOutputPath, "resRandRecHCR6.RData"))
metricsHCR6 <- metrics %>% mutate(mngtStrategy = "Fmsy1040")

results <- rbind(metricsHCR0, 
                 metricsHCR1,
                 metricsHCR6)

results$mngtStrategy <- factor(results$mngtStrategy,
                               levels = c("NoCatch", "Current", "Fmsy1040"))

ggplot(data = results, aes(x = mngtStrategy, y = closureFreq)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(data = results, aes(x = mngtStrategy, y = meanB1plus)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(data = results, aes(x = mngtStrategy, y = meanCat)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(data = results, aes(x = mngtStrategy, y = sdCat)) + 
  geom_boxplot() +
  theme_minimal()



