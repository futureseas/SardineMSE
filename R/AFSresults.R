# compare performance metrics among scenarios

library(tidyverse)

noCat <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/nocatchrrmetrics.csv")
noCatBio <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/nocatchrrsmryBio.csv")

pikitch0.8Bthresh <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/pickrr4metrics.csv")
pikitch0.8BthreshBio <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/pickrr4smryBio.csv")
pikitch0.8BthreshCat <-read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/pickrr4smryCat.csv")
pikitch0.8BthreshBio <- inner_join(pikitch0.8BthreshBio, pikitch0.8BthreshCat)

pikitchFmsy <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/pickmsyrrmetrics.csv")
pikitchFmsyBio <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/pickmsyrrsmryBio.csv")
pikitchFmsyCat <-read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/pickmsyrrsmryCat.csv")
pikitchFmsyBio <- inner_join(pikitchFmsyBio, pikitchFmsyCat)

Fmsy1040 <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/1040msyrrmetrics.csv")
Fmsy1040Bio <- read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/1040msyrrsmryBio.csv")
Fmsy1040Cat <-read.csv("C:/Users/rwildermuth/Documents/FutureSeas/AFSresults/1040msyrrsmryCat.csv")
Fmsy1040Bio <- inner_join(Fmsy1040Bio, Fmsy1040Cat)

merge(pikitch0.8BthreshBio, pikitchFmsyBio, by = "iter")
merge(pikitch0.8BthreshBio, Fmsy1040Bio, by = "iter")
merge(pikitch0.8BthreshBio, noCatBio, by = "iter")

dim(pikitch0.8Bthresh)
dim(pikitchFmsy)
dim(noCat)

pikitch0.8Bthresh$mngtStrategy <- "Pikitch0.8"
pikitchFmsy$mngtStrategy <- "PikitchFmsy"
noCat$mngtStrategy <- "NoCatch"
Fmsy1040$mngtStrategy <- "Fmsy1040"

results <- rbind(pikitch0.8Bthresh, 
                 pikitchFmsy[sample(pikitchFmsy$X, nrow(pikitch0.8Bthresh)),],
                 noCat[sample(noCat$X, nrow(pikitch0.8Bthresh)),],
                 Fmsy1040[sample(Fmsy1040$X, nrow(pikitch0.8Bthresh)),])

results$mngtStrategy <- factor(results$mngtStrategy,
                               levels = c("NoCatch", "Pikitch0.8", "PikitchFmsy", "Fmsy1040"))

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


# See how iterations compare ----------------------------------------------

noCatBio <- noCatBio %>% mutate(meanCat = NA, sdCat = NA)

noCatPik0.8 <- inner_join(noCatBio, pikitch0.8BthreshBio, by = "iter") %>% 
                  mutate(compared = "noCatPik0.8")
noCatPikFmsy <- merge(noCatBio, pikitchFmsyBio, by = "iter") %>% 
                  mutate(compared = "noCatPikFmsy")
noCat1040 <- merge(noCatBio, Fmsy1040Bio, by = "iter") %>% 
                mutate(compared = "noCatFmsy1040")

Pik0.81040 <- inner_join(Fmsy1040Bio, pikitch0.8BthreshBio, by = "iter") %>% 
                mutate(compared = "Fmsy1040Pik0.8")
PikFmsy1040 <- inner_join(Fmsy1040Bio, pikitchFmsyBio, by = "iter") %>% 
                  mutate(compared = "Fmsy1040PikFmsy")

PikFmsyPik0.8 <- inner_join(pikitchFmsyBio, pikitch0.8BthreshBio, by = "iter") %>% 
                    mutate(compared = "PikFmsyPik0.8")

compareStrategies <- rbind(noCatPik0.8, noCatPikFmsy, noCat1040,
                           Pik0.81040, PikFmsy1040, PikFmsyPik0.8)
compareStrategies <- compareStrategies %>% 
                        mutate(closureLimit = (closuresN.y - closuresN.x)/51,
                               biomDiff = meanB1plus.y - meanB1plus.x,
                               catchDiff = meanCat.y - meanCat.x,
                               sdCatDiff = sdCat.y - sdCat.x) %>%
                        select(iter, biomDiff, closureLimit, catchDiff, sdCatDiff, compared) %>%
                        pivot_longer(cols = c(biomDiff, closureLimit, catchDiff, sdCatDiff), 
                                     names_to = "metric", values_to = "difference")

compareStrategies %>% 
  filter(metric %in% c("biomDiff", "closureLimit")) %>%
  ggplot(aes(x = metric, y = difference)) +
  geom_dotplot(binaxis = "y",
               stackdir = "center") +
  facet_wrap(~compared)

compareStrategies %>% 
  filter(metric %in% c("catchDiff", "sdCatDiff")) %>%
  ggplot(aes(x = metric, y = difference)) +
  geom_dotplot(binaxis = "y",
               stackdir = "center") +
  facet_wrap(~compared)
