# Code to make plots for the Sardine MSE manuscript
# Created: 9/7/2022, Robert Wildermuth

library(r4ss)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(cowplot)

source("../SardineMSE/R/CalcPerformance.R")
source("../SardineMSE/R/CalcTermTS.R")
source("../SardineMSE/R/CalcRelMetric.R")
source("../SardineMSE/R/CalcRebuildTime.R")
source("../SardineMSE/R/CalcRelErr.R")

# memory.limit(size = 30000)

# Read in data and calculate metrics --------------------------------------

# mseDir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios"
mseDir <- "J:/Desiree/Sardine/SardineScenarios"
termYr <- 2068

smryOutputList <- readRDS(file.path(mseDir, 
                                    "serverRegARPDOcyclPDOclimSSTMICE_allHCRs_addlResults.RDS"))
                                    # "serverRegARtest_allHCRs_addlResults.RDS"))

performanceList <- CalcPerformance(smryOutputList)

metricsTbl <- performanceList$perfomanceMetrics

# Clear up some memory
smryOutputList$lenComp <- NULL
smryOutputList$ageComp <- NULL

# parse out HCR and recruitment scenario
metricsTbl <- metricsTbl %>% mutate(nameHCR = case_when(HCR == "HCR0" ~ "NoCat",
                                                        HCR == "HCR1" ~ "PFMCF018",
                                                        HCR == "HCR2" ~ "PFMCFSST",
                                                        HCR == "HCR3" ~ "ConstF",
                                                        HCR == "HCR5" ~ "Pikitch",
                                                        HCR == "HCR6" ~ "40-10",
                                                        HCR == "HCR7" ~ "DynPik",
                                                        HCR == "HCR8" ~ "Dyn40-10",
                                                        HCR == "HCR9" ~ "Index"))

# get terminal estimates of these values for timeseries plots
termTS <- CalcTermTS(smryOutputList, termYr = termYr) %>%
            mutate(nameHCR = case_when(HCR == "HCR0" ~ "NoCat",
                                       HCR == "HCR1" ~ "PFMCF018",
                                       HCR == "HCR2" ~ "PFMCFSST",
                                       HCR == "HCR3" ~ "ConstF",
                                       HCR == "HCR5" ~ "Pikitch",
                                       HCR == "HCR6" ~ "40-10",
                                       HCR == "HCR7" ~ "DynPik",
                                       HCR == "HCR8" ~ "Dyn40-10",
                                       HCR == "HCR9" ~ "Index"),
                   recScen = factor(recScen, levels = c("ARRec", "PDOcyclRec", 
                                                        "MICERec", "PDOclimRec",
                                                        "RegRec", "SSTRec")))
                       

omName <- grep("_OM", smryOutputList$tsSmry$model_run,
               fixed = TRUE, value = TRUE)[1]
hcrPal <- brewer.pal(11, "Set3")[-2]

hcrLabels <- c("NoCat", "PFMCF018", "PFMCFSST", "ConstF", "Pikitch", "40-10",
               "DynPik", "Dyn40-10", "Index")

# define reference scenarios to use in plot calculations
refScens <- c("ARRec", "PDOcyclRec", "MICERec", "PDOclimRec")

# timeseries plot for presentation
sampleDat <- termTS %>% filter(model_run == omName, 
                               recScen %in% refScens,
                               HCR == "HCR0") %>%
  #mutate(recScen = factor(recScen, levels = refScens)) %>%
  select(Bio_smry, year, model_run, iteration, scenario, HCR, recScen) %>%
  # filter(year > 2019, year != 2070) %>%
  # mutate(year = signif(year-4.5, digits = 3)) %>%
  group_by(year, 
           scenario, recScen, 
           HCR) %>%
  summarize(meanAge1Plus = mean(Bio_smry),
            medAge1Plus = median(Bio_smry),
            lowAge1Plus = quantile(Bio_smry, probs = 0.05, na.rm = TRUE),
            hiAge1Plus = quantile(Bio_smry, probs = 0.95, na.rm = TRUE),
            # add thresholds also
            denom = n(),
            less50k = sum(Bio_smry < 50000)/denom,
            more50kless150k = sum(Bio_smry > 50000 & 
                                    Bio_smry < 150000)/denom,
            more150kless400k = sum(Bio_smry > 150000 & 
                                     Bio_smry < 400000)/denom,
            more400k = sum(Bio_smry > 400000)/denom) 

itSamp <- sample(unique(termTS$iteration), size = 3)

p1 <- sampleDat %>%
  ggplot(aes(x = year, y = meanAge1Plus)) +
  geom_vline(xintercept = 2019, color = "gray", linetype = "dashed") +
  geom_line() +
  geom_ribbon(aes(ymin = lowAge1Plus, ymax = hiAge1Plus, alpha = 0.3)) +
  facet_grid(rows = vars(HCR), cols = vars(recScen)) +
  theme_classic() +
  labs(x = NULL, y = "Age 1+ Biomass (mt)") +
       # caption = "Mean (solid line), 80% CI (grey shaded ribbon), and five sample trajectories under a No Catch (HCR0) management rule\n for each recruitment scenario.") +
  # add example trajectories
  geom_line(data = subset(termTS, model_run == omName & 
                            HCR == "HCR0" & 
                            iteration %in% itSamp & 
                            recScen %in% refScens),
            mapping = aes(x = year, y = Bio_smry, 
                          linetype = as.character(iteration), 
                          color = as.character(iteration)),
            size = 0.5) +
  scale_linetype_manual(values = rep("solid", 5)) +
  scale_color_manual(values = brewer.pal(n = 5, "Pastel1")) +
  geom_line(data = sampleDat, aes(x = year, y = meanAge1Plus)) +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        strip.text.y = element_blank(),
        panel.grid = element_blank()) +
  geom_hline(yintercept = 150000, color = "red") 

p1alt <- termTS %>% filter(model_run == omName, HCR == "HCR0",
                           recScen %in% refScens) %>%
  ggplot(aes(x = year, y = Bio_smry)) +
  geom_vline(xintercept = 2019, color = "gray", linetype = "dashed") +
  geom_line(aes(linetype = as.character(iteration)), color = "grey",alpha = 0.4) +
  # geom_ribbon(aes(ymin = lowAge1Plus, ymax = hiAge1Plus, alpha = 0.3)) +
  facet_grid(rows = vars(HCR), cols = vars(recScen)) +
  theme_classic() +
  labs(x = NULL, y = "Age 1+ Biomass (mt)") +
  # # caption = "Mean (solid line), 80% CI (grey shaded ribbon), and five sample trajectories under a No Catch (HCR0) management rule\n for each recruitment scenario.") +
  # # add example trajectories
  geom_line(data = subset(termTS, model_run == omName &
                            HCR == "HCR0" &
                            iteration %in% itSamp &
                            recScen %in% refScens),
            mapping = aes(x = year, y = Bio_smry,
                          linetype = as.character(iteration),
                          color = as.character(iteration)),
            size = 0.5) +
  scale_linetype_manual(values = rep("solid", 500)) +
  # scale_color_manual(values = brewer.pal(n = 5, "Pastel1")) +
  geom_line(data = sampleDat, aes(x = year, y = medAge1Plus)) +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        strip.text.y = element_blank(),
        panel.grid = element_blank()) +
  geom_hline(yintercept = 150000, color = "red") +
  coord_cartesian(ylim = c(0, 2e7))
  

# sample recruitment deviations
p2 <- termTS %>% filter(model_run == omName,
                  HCR == "HCR0", iteration %in% itSamp,
                  recScen %in% refScens) %>%
  ggplot(aes(x = year, y = rec_dev)) +
  geom_line(aes(linetype = as.character(iteration), 
                color = as.character(iteration))) + 
  scale_linetype_manual(values = rep("solid", 50)) +
  # scale_color_manual(values = brewer.pal(n = 5, "Pastel1")) +
  guides(linetype = "none") +
  geom_hline(yintercept = 0, color = "grey") +
  facet_grid(rows = vars(iteration), cols = vars(recScen)) + 
  theme_classic() +
  geom_vline(xintercept = 2019, color = "gray", linetype = "dashed") +
  labs(x = "Year", y = "Recruitment Deviation") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.y = element_blank())

# From: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# also see section on gtable() and Aligning plot panels
grid.arrange(p1, p2)
ggdraw() +
  draw_plot(p1, x = 0, y = 0.5, width = 1) +
  draw_plot(p2, x = 0, y = 0, width = 1, height = 0.5)
plot_grid(p1alt, p2, ncol = 1, nrow = 2, align = "vh", axis = "tblr")

# histogram of rec devs
termTS %>% filter(model_run == omName, HCR == "HCR0", year > 2019) %>%
  ggplot(aes(x = rec_dev)) +
  geom_histogram() + 
  facet_wrap(vars(recScen)) +
  geom_vline(xintercept = c(-1.25,0,1.25), color = "grey") +
  theme_minimal()

exIt <- 42#75
exBio <- termTS %>% filter(model_run == omName, recScen == "MICERec", iteration == exIt) %>%
  ggplot(aes(x = year, y = Bio_smry)) +
  geom_hline(yintercept = c(150000, 50000), color = "red") +
  geom_line(aes(color = HCR), size = 1) +
  scale_color_manual(values = hcrPal, labels = hcrLabels) +
  # 
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
  theme_classic() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = "Year", y = "Age1+ Biomass (mt)")

exRec <- termTS %>% filter(model_run == omName, recScen == "MICERec", iteration == exIt) %>%
  ggplot(aes(x = year, y = rec_dev)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(aes(x = year, y = rec_dev)) +
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
  geom_point(data = termTS %>% filter(model_run == omName,
                                    recScen == "MICERec", iteration == exIt,
                                    rec_dev < -1.25),
           mapping = aes(x = year, y = rec_dev), show.legend = FALSE) +
  theme_classic() +
  labs(x = "Year", y = "Recruitment Deviation")

exObsCat <- smryOutputList$obsCatch %>% filter(recScen == "MICERec", 
                                               model_run == "constGrowBothShort_EM_2068",
                                               iteration == exIt, HCR == "HCR2",
                                               year > 0) %>%
              group_by(year, model_run) %>%
              summarize(catch = sum(catch))

exCat <- termTS %>% filter(model_run == omName, recScen == "MICERec", 
                  iteration == exIt) %>%
  ggplot(aes(x = year, y = totCatch)) +
  geom_line(aes(color = HCR), size = 1) +
  scale_color_manual(values = hcrPal, labels = hcrLabels) +
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = 200000, color = "grey") +
  # geom_point(data = exObsCat, shape = 0,
  #            mapping = aes( x = year, y = catch), show.legend = FALSE) +
  theme_classic() +
  labs(x = "Year", y = "Catch (mt)")

exObsInd <- smryOutputList$obsCPUE %>% filter(recScen == "MICERec", index == 4,
                                               model_run == "constGrowBothShort_EM_2068",
                                               iteration == exIt, HCR == "HCR2",
                                               year > 0) %>%
              left_join(termTS %>% filter(model_run == omName, 
                                          recScen == "MICERec", 
                                          iteration == exIt,
                                          HCR == "HCR2"), 
                        by = c("year", "iteration", "HCR", "recScen")) %>%
              mutate(errSmryBio = ((obs - Bio_smry)/Bio_smry)*100)

# NOTE: need to run error section to get 'termRE' first
# exRE <- termRE %>% filter(recScen == "MICERec",
#                   iteration == exIt, HCR == "HCR2") %>%
#   ggplot(aes(x = year, y = errSmryBio)) +
#   geom_line() +
#   geom_hline(yintercept = 0, color = "grey", linetype = "solid") +
#   geom_point(data = exObsInd, shape = 0, show.legend = FALSE) +
#   theme_classic() +
#   geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
#   labs(x = "Year", y = "Relative Error (%)")

plot_grid(exBio, exRec, exCat, exRE, ncol = 2, nrow = 2, align = "vh", axis = "tblr")


hLines <- data.frame(output = c("Bio_smry", "Bio_smry", "rec_dev"),
                     yinter = c(150000, 50000, 0))
termTS %>% filter(HCR == "HCR0", recScen == "ARRec", iteration == 8) %>% 
  pivot_longer(cols = c(Bio_smry, rec_dev), names_to = "output") %>%
  ggplot(aes(x = year, y = value)) +
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
  geom_hline(data = hLines,
             aes(yintercept = yinter), color = c("red", "red", "grey")) +
  geom_line(aes(color = output), size = 1) +
  facet_wrap(~output, scales = "free_y", ncol = 1,
              strip.position = "left",
              labeller = as_labeller(c(Bio_smry = "Age1+ Biomass (log-mt)", 
                                       rec_dev = "Recruitment Deviation"))) +
  theme_classic() +
  theme(legend.position = "none",
        strip.placement = "outside",
        strip.background = element_blank()) +
  labs(x = "Year", y = "")

# example of EM error progression
errProgEx <- smryOutputList$tsSmry %>% filter(grepl("ARRecHCR1", scenario, fixed = TRUE), 
                                              iteration == 8, Seas == 1) %>%
              mutate(emYear = as.numeric(regmatches(model_run,
                                                    gregexpr("[[:digit:]]+", 
                                                             model_run))))%>%
              mutate(emYear = case_when(is.na(emYear) ~ 2019,
                                        TRUE ~ emYear)) %>%
              filter(year <= emYear, model_run != omName) 

emErrPlot1 <- errProgEx %>% filter(emYear == 2045) %>%
  ggplot(aes(y = Bio_smry, x = year, color = emYear, linetype = as.character(model_run))) +
  geom_line(size = 1) +
  scale_linetype_manual(values = rep("solid", 51)) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_line(data = termTS %>% filter(model_run == omName, iteration == 8, 
                                     grepl("ARRecHCR1", scenario, fixed = TRUE)),
            color = "orangered", size = 1) +
  labs(x = "", y = "") +
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed")

emErrPlot2 <- errProgEx %>%
  ggplot(aes(y = Bio_smry, x = year, color = emYear, linetype = as.character(model_run))) +
  geom_line(size = 1) +
  scale_linetype_manual(values = rep("solid", 52)) +
  geom_point(data = termTS %>% filter(model_run != omName, iteration == 8, 
                                      grepl("ARRecHCR1", scenario, fixed = TRUE),
                                      year > 2019),
             color = "chartreuse3", shape = 4, size = 1.5, stroke = 2) +
  geom_line(data = termTS %>% filter(model_run == omName, iteration == 8, 
                                     grepl("ARRecHCR1", scenario, fixed = TRUE)),
            color = "orangered", size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Age1+ Biomass (mt)", x = "")+
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed")

emErrPlot3 <- termTS %>% filter(model_run != omName, iteration == 8, 
                                      grepl("ARRecHCR1", scenario, fixed = TRUE)) %>%
  ggplot(aes(y = Bio_smry, x = year, linetype = as.character(model_run))) +
  scale_linetype_manual(values = rep("solid", 3)) +
  geom_line(color = "chartreuse3", size = 1) +
  geom_line(data = termTS %>% filter(model_run == omName, iteration == 8, 
                                     grepl("ARRecHCR1", scenario, fixed = TRUE)),
            color = "orangered", size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Year", y="")+
  geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed")

grid.arrange(emErrPlot1, emErrPlot2, emErrPlot3)
# Metrics plots -----------------------------------------------------------

# Calculate relative biomass and catch metrics
annualRelBioCat <- CalcRelMetric(metricData = termTS %>% 
                                   filter(year > 2019, year != 2070,
                                          model_run == omName), 
                                 metricCol = "Bio_smry") %>%
                      full_join(y = CalcRelMetric(metricData = termTS %>% 
                                                    filter(year > 2019, 
                                                           year != 2070,
                                                           model_run == omName), 
                                                  metricCol = "totCatch"),
                                by = c("Bio_smry", "rec_dev", "year", "Seas", 
                                       "model_run", "iteration", "scenario", 
                                       "HCR", "recScen", "nameHCR", "totCatch",  
                                       "Value.Recr", "Value.SSB", "emYear")) %>%
                      select(!contains("Metric"))

# Add grouping variable for non/climate scenarios
annualRelBioCat <- annualRelBioCat %>% 
  mutate(climGroup = case_when(recScen %in% c("ARRec", "PDOcyclRec", "RegRec") ~ "noClim",
                               recScen %in% c("MICERec", "PDOclimRec", "SSTRec") ~ "clim"),
         nameHCR = factor(nameHCR, levels = c("NoCat", "PFMCF018", "PFMCFSST", "ConstF",
                                              "Index", "Pikitch", "40-10", "DynPik", "Dyn40-10")))

# plot annual relative biomass metrics
relBioAll <- annualRelBioCat %>% filter(recScen %in% refScens) %>%
  group_by(nameHCR, climGroup) %>%
  summarize(meanRelBio = mean(relBio_smryMean),
            medRelBio = median(relBio_smryMean),
            hiRelBio = quantile(relBio_smryMean, probs = 0.95),
            loRelBio = quantile(relBio_smryMean, probs = 0.05),
            q1RelBio = quantile(relBio_smryMean, probs = 0.25),
            q3RelBio = quantile(relBio_smryMean, probs = 0.75),
            nRelBio = n()) %>% 
  ggplot(aes(x = nameHCR, fill = climGroup)) +
  geom_boxplot(aes(ymin = loRelBio, lower = q1RelBio, middle = medRelBio, 
                   upper = q3RelBio, ymax = hiRelBio),
               stat = "identity", position = position_dodge(0.95)) +
  # ggplot(aes(x = HCR, y = relAnnBioMax)) +
  # #geom_hline(yintercept = c(50000, 150000), color = "red") +
  # # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  # geom_boxplot(aes(fill = HCR), outlier.shape = NA) +
  # coord_cartesian(ylim = c(0,2.5)) +
  # geom_jitter(aes(color = recScen, alpha = 0.03)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  #scale_fill_manual(values = hcrPal, labels = hcrLabels, name = "HCR") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(y = "Annual Age1+ Biomass:Mean", x = "HCR") 

relBioColl <- annualRelBioCat %>% filter(recScen %in% refScens) %>%
  filter(Bio_smry <= 50000) %>%
  ggplot(aes(x = HCR, y = relBio_smryMean)) +
  #geom_hline(yintercept = c(50000, 150000), color = "red") +
  geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  # geom_jitter(aes(color = recScen, alpha = 0.03)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Annual Age1+ Biomass:Mean", x = "HCR") 

relBioClose <- annualRelBioCat %>% filter(recScen %in% refScens) %>%
  filter(Bio_smry <= 150000) %>%
  ggplot(aes(x = HCR, y = relBio_smryMean)) +
  #geom_hline(yintercept = c(50000, 150000), color = "red") +
  geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  # geom_jitter(aes(color = recScen, alpha = 0.03)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Annual Age1+ Biomass:Mean", x = "HCR") 

sever <- metricsTbl %>% filter(recScen %in% refScens) %>%
  group_by(nameHCR) %>%
  summarize(meanCollSevr = mean(meanCollapseSever),
            medCollSevr = median(meanCollapseSever),
            hiCollSevr = quantile(meanCollapseSever, probs = 0.95),
            loCollSevr = quantile(meanCollapseSever, probs = 0.05),
            q1CollSevr = quantile(meanCollapseSever, probs = 0.25),
            q3CollSevr = quantile(meanCollapseSever, probs = 0.75),
            nCollSevr = n()) %>% 
  ggplot(aes(x = nameHCR, fill = nameHCR)) +
  geom_boxplot(aes(ymin = loCollSevr, lower = q1CollSevr, middle = medCollSevr, 
                   upper = q3CollSevr, ymax = hiCollSevr),
               stat = "identity") +
  # ggplot(aes(x = HCR, y = meanCollapseSever)) +
  # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels, name = "HCR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Collapse severity", x = "HCR") 

relCatAll <- annualRelBioCat %>% filter(HCR != "HCR0", recScen %in% refScens) %>%
  #filter(Bio_smry <= 150000) %>%
  group_by(nameHCR, climGroup) %>%
  summarize(meanRelCat = mean(reltotCatchMean),
            medRelCat = median(reltotCatchMean),
            hiRelCat = quantile(reltotCatchMean, probs = 0.95),
            loRelCat = quantile(reltotCatchMean, probs = 0.05),
            q1RelCat = quantile(reltotCatchMean, probs = 0.25),
            q3RelCat = quantile(reltotCatchMean, probs = 0.75),
            nRelCat = n()) %>% 
  ggplot(aes(x = nameHCR, fill = climGroup)) +
  geom_boxplot(aes(ymin = loRelCat, lower = q1RelCat, middle = medRelCat, 
                   upper = q3RelCat, ymax = hiRelCat),
               stat = "identity", position = position_dodge(0.95)) +
  # ggplot(aes(x = HCR, y = relAnnCatMax)) +
  # #geom_hline(yintercept = c(50000, 150000), color = "red") +
  # # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  # geom_boxplot(aes(fill = HCR), outlier.shape = NA) +
  # coord_cartesian(ylim = c(0,2.5)) +
  # geom_jitter(aes(color = recScen, alpha = 0.03)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  #scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1], name = "HCR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Annual Catch:Mean", x = "HCR") 

sdCatAll <- metricsTbl %>%  filter(HCR != "HCR0", recScen %in% refScens) %>%
  group_by(nameHCR) %>%
  summarize(meanRelCatSD = mean(sdCatch),
            medRelCatSD = median(sdCatch),
            hiRelCatSD = quantile(sdCatch, probs = 0.95),
            loRelCatSD = quantile(sdCatch, probs = 0.05),
            q1RelCatSD = quantile(sdCatch, probs = 0.25),
            q3RelCatSD = quantile(sdCatch, probs = 0.75),
            nRelCatSD = n()) %>% 
  ggplot(aes(x = nameHCR, fill = nameHCR)) +
  geom_boxplot(aes(ymin = loRelCatSD, lower = q1RelCatSD, middle = medRelCatSD, 
                   upper = q3RelCatSD, ymax = hiRelCatSD),
               stat = "identity") +
  # ggplot(aes(x = HCR, y = sdCatch)) +
  # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1], name = "HCR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Catch SD (mt)", x = "HCR")

closeFreq <- metricsTbl %>% filter(recScen %in% refScens) %>%
  group_by(nameHCR) %>%
  summarize(meanFrqClose = mean(closuresFreq),
            medFrqClose = median(closuresFreq),
            hiFrqClose = quantile(closuresFreq, probs = 0.95),
            loFrqClose = quantile(closuresFreq, probs = 0.05),
            q1FrqClose = quantile(closuresFreq, probs = 0.25),
            q3FrqClose = quantile(closuresFreq, probs = 0.75),
            nFrqClose = n()) %>% 
  ggplot(aes(x = nameHCR, fill = nameHCR)) +
  geom_boxplot(aes(ymin = loFrqClose, lower = q1FrqClose, middle = medFrqClose, 
                   upper = q3FrqClose, ymax = hiFrqClose),
               stat = "identity") +
  # ggplot(aes(x = HCR, y = closuresFreq)) +
  # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  # facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels, name = "HCR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Frequency Bt < 150,000 mt", x = "HCR")

collapseFreq <- metricsTbl %>% filter(recScen %in% refScens) %>%
  group_by(nameHCR) %>%
  summarize(meanFrqColl = mean(collapseFreq),
            medFrqColl = median(collapseFreq),
            hiFrqColl = quantile(collapseFreq, probs = 0.95),
            loFrqColl = quantile(collapseFreq, probs = 0.05),
            q1FrqColl = quantile(collapseFreq, probs = 0.25),
            q3FrqColl = quantile(collapseFreq, probs = 0.75),
            nFrqColl = n()) %>% 
  ggplot(aes(x = nameHCR, fill = nameHCR)) +
  geom_boxplot(aes(ymin = loFrqColl, lower = q1FrqColl, middle = medFrqColl, 
                   upper = q3FrqColl, ymax = hiFrqColl),
               stat = "identity") +
  # ggplot(aes(x = HCR, y = collapseFreq)) +
  # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  # facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels, name = "HCR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Frequency Bt < 50,000 mt", x = "HCR")

# Calculate rebuilding
rebuildTime <- CalcRebuildTime(tsSmry = performanceList$tsSmry) %>% 
                  mutate(nameHCR = case_when(HCR == "HCR0" ~ "NoCat",
                                             HCR == "HCR1" ~ "PFMCF018",
                                             HCR == "HCR2" ~ "PFMCFSST",
                                             HCR == "HCR3" ~ "ConstF",
                                             HCR == "HCR5" ~ "Pikitch",
                                             HCR == "HCR6" ~ "40-10",
                                             HCR == "HCR7" ~ "DynPik",
                                             HCR == "HCR8" ~ "Dyn40-10",
                                             HCR == "HCR9" ~ "Index"))

# look at how many iterations never rebuilt
performanceList$tsSmry %>% filter(closureLength == 50) %>% group_by(HCR, recScen)%>% summarize(nvrRebuiltN = n())
# only 1 iteration in reference set didn't rebuild (iter 77) except under HCR1

rebuild <- rebuildTime %>% filter(recScen %in% refScens) %>% 
  group_by(nameHCR) %>%
  summarize(meanRebuild = mean(meanRebuildTime),
            medRebuild = median(medRebuildTime),
            hiRebuild = quantile(medRebuildTime, probs = 0.95),
            loRebuild = quantile(medRebuildTime, probs = 0.05),
            q1Rebuild = quantile(medRebuildTime, probs = 0.25),
            q3Rebuild = quantile(medRebuildTime, probs = 0.75),
            nRebuild = n()) %>% 
  ggplot(aes(x = nameHCR, fill = nameHCR)) +
  geom_boxplot(aes(ymin = loRebuild, lower = q1Rebuild, middle = medRebuild, 
                   upper = q3Rebuild, ymax = hiRebuild),
               stat = "identity") +
  # ggplot(aes(x = HCR, y = meanRebuildTime)) +
  # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels, name = "HCR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Mean Rebuilding Time (yrs)", x = "HCR")

grid.arrange(closeFreq, rebuild, collapseFreq, sever)

rebuildTime %>%  filter(recScen %in% refScens) %>% 
  ggplot(aes(x = HCR, y = nClosures)) +
  geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
  #facet_wrap(~recScen, scales = "free") + 
  theme_minimal() +
  scale_fill_manual(values = hcrPal, labels = hcrLabels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Number of Closures", x = "HCR")

rebuildTime %>%  filter(recScen %in% refScens) %>%
  group_by(HCR) %>%
  summarize(meanNclose = mean(nClosures),
            medNclose = median(nClosures),
            high90 = quantile(nClosures, probs = 0.9))
  
# calculate frequency of TAC = 0
noCatFreq <- annualRelBioCat %>% mutate(TAC0 = totCatch == 0) %>%
                group_by(model_run, iteration, scenario, HCR, recScen) %>%
                summarize(yrsN = n(),
                          nocatFreq = sum(TAC0)/yrsN)

# join tables for clean faceted plots
metricsTbl <- metricsTbl %>% left_join(y = rebuildTime, 
                                       by = c("iteration", "scenario", "nameHCR",
                                              "model_run", "HCR", "recScen")) %>%
                left_join(y = noCatFreq, 
                          by = c("iteration", "scenario", "yrsN",
                                 "model_run", "HCR", "recScen")) %>%
                # Add grouping variable for non/climate scenarios
                mutate(climGroup = case_when(recScen %in% c("ARRec", "PDOcyclRec", 
                                                            "RegRec") ~ "noClim",
                                             recScen %in% c("MICERec", "PDOclimRec", 
                                                            "SSTRec") ~ "clim"),
                       nameHCR = factor(nameHCR, levels = c("NoCat", "PFMCF018", "PFMCFSST", "ConstF",
                                                            "Index", "Pikitch", "40-10", "DynPik", "Dyn40-10")))

metricLabs <- c("Frequency Bt < 150,000 mt", "Frequency Bt < 50,000 mt",
                "Frequency of TAC = 0 mt",
                "Mean Collapse Severity", "Number of Cutoffs", 
                "Mean Rebuilding Length (yrs)", "Mean Collapse Length (yrs)", "Catch SD (mt)",
                "Mean Minimum Age", "Mean Minimum Length")
names(metricLabs) <- c("closuresFreq", "collapseFreq", "nocatFreq", "meanCollapseSever",
                         "nClosures", "meanRebuildTime", "meanCollapseTime", "sdCatch",
                       "minAge", "minLen")

fishMetPlots <- metricsTbl %>% filter(recScen %in% refScens) %>%
  select(iteration, scenario, model_run, HCR, nameHCR, recScen, closuresFreq, 
         collapseFreq, nocatFreq, meanCollapseSever, nClosures, meanRebuildTime, meanCollapseTime, sdCatch,
         #minAge, minLen,
         climGroup) %>%
  pivot_longer(cols = c(closuresFreq, collapseFreq, nocatFreq, meanCollapseSever, nClosures,
                        meanRebuildTime, meanCollapseTime, sdCatch), #, minAge, minLen),
               names_to = "Metric", values_to = "vals") %>%
  group_by(nameHCR, Metric, climGroup) %>%
  summarize(meanMetric = mean(vals, na.rm = TRUE),
            medMetric = median(vals, na.rm = TRUE),
            hiMetric = quantile(vals, probs = 0.975, na.rm = TRUE),
            loMetric = quantile(vals, probs = 0.025, na.rm = TRUE),
            q1Metric = quantile(vals, probs = 0.25, na.rm = TRUE),
            q3Metric = quantile(vals, probs = 0.75, na.rm = TRUE),
            nMetric = n()) %>% 
  #filter(!Metric %in% c("minAge", "minLen")) %>%
  ggplot(aes(x = nameHCR, fill = climGroup)) +
  geom_boxplot(aes(ymin = loMetric, lower = q1Metric, middle = medMetric, 
                   upper = q3Metric, ymax = hiMetric),
               stat = "identity", position = position_dodge(0.95)) +
  facet_wrap(~Metric, scales = "free_y",
             strip.position = "left",
             labeller = labeller(Metric = metricLabs),
             ncol = 3) +
  theme_classic() +
  scale_fill_manual(values = hcrPal[c(3,1)], labels = c("Climate", "No Climate"), name = "Scenario\nGroup") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  labs(x = "HCR")

bioCatPlots <- annualRelBioCat %>% filter(recScen %in% refScens) %>%
  select(iteration, scenario, model_run, HCR, nameHCR, recScen, climGroup,
         relBio_smryMean, reltotCatchMean) %>%
  pivot_longer(cols = c(relBio_smryMean, reltotCatchMean),
               names_to = "Metric", values_to = "vals") %>%
  group_by(nameHCR, Metric, climGroup) %>%
  summarize(meanMetric = mean(vals, na.rm = TRUE),
            medMetric = median(vals, na.rm = TRUE),
            hiMetric = quantile(vals, probs = 0.975, na.rm = TRUE),
            loMetric = quantile(vals, probs = 0.025, na.rm = TRUE),
            q1Metric = quantile(vals, probs = 0.25, na.rm = TRUE),
            q3Metric = quantile(vals, probs = 0.75, na.rm = TRUE),
            nMetric = n()) %>% 
  ggplot(aes(x = nameHCR, fill = climGroup)) +
  geom_boxplot(aes(ymin = loMetric, lower = q1Metric, middle = medMetric, 
                   upper = q3Metric, ymax = hiMetric),
               stat = "identity", position = position_dodge(0.95)) +
  facet_wrap(~Metric, scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(relBio_smryMean = "Annual Age1+ Biomass:Mean",
                                      reltotCatchMean = "Annual Catch:Mean")),
             ncol = 2) +
  theme_classic() +
  scale_fill_manual(values = hcrPal[c(3,1)], labels = c("Climate", "No Climate"), name = "Scenario\nGroup") +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "HCR")

grid.arrange(bioCatPlots, fishMetPlots)
# Error metrics -----------------------------------------------------------

metricsTbl %>% filter(recScen %in% refScens, !is.na(frqNonConvg)) %>% 
  group_by(HCR, recScen) %>%
  summarize(maxNonConvrg = max(frqNonConvg, na.rm = TRUE), 
            minNonConvrg = min(frqNonConvg, na.rm = TRUE)) %>%
  print(n = 28)

# Want to subset error metrics by years and iterations with poor recruitment/biomass status

convrgCheck <- smryOutputList$sclSmry %>% 
  select(max_grad, model_run, iteration, scenario) %>%
  mutate(emYear = as.numeric(regmatches(model_run,
                                        gregexpr("[[:digit:]]+", 
                                                 model_run))))

# Bring in simulated data series used for HCR9 biomass ests
simBioObsHCR9 <- smryOutputList$obsCPUE %>%
                  mutate(emYear = as.numeric(regmatches(model_run,
                                                        gregexpr("[[:digit:]]+", 
                                                                 model_run))),
                         plotGroup = "ATsurvey") %>%
                  # need to filter to data used in final assessment per MSE run
                  filter(emYear == termYr, index == 4, year > 2019, HCR == "HCR9") %>%
                  select(year, obs, emYear, model_run, iteration, scenario, HCR, 
                         recScen, plotGroup) %>%
                  rename(Bio_smry = obs)

 cnvrgTS <- smryOutputList$tsSmry  %>%
  left_join(y = convrgCheck, by = c("iteration", "model_run", "scenario")) %>%
  mutate(plotGroup = case_when(model_run == omName ~ "OM",
                               max_grad > 0.01 ~ "non-convrg",
                               max_grad < 0.01 ~ "convrg"), 
         HCR = sub(pattern = ".*Rec","", scenario),
         recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*EM_","", recScen))

errCompare <- cnvrgTS %>% filter(Seas == 1, model_run != omName) %>%
  select(Bio_smry, year, model_run, iteration, scenario, HCR, 
         recScen, emYear, plotGroup) %>%
  bind_rows(simBioObsHCR9) %>%
  inner_join(y = subset(termTS, model_run == omName), 
             by = c("year", "iteration", "scenario", "HCR", "recScen")) %>%
  #filter(iteration == 1) %>%
  rename(age1plusOM = Bio_smry.y,
         age1plusEM = Bio_smry.x) %>% 
  mutate(errSmryBio = ((age1plusEM - age1plusOM)/age1plusOM)*100,
         recDevState = case_when(rec_dev <= -1.25 ~ "poor",
                                 rec_dev >= 1.25 ~ "high",
                                 TRUE ~ "avg"))

errCompare <- CalcRelErr(smryOutputList = smryOutputList,
                         termTS = termTS,
                         termYr = termYr)

# Error among assessments within year
#annualRE <- errCompare %>% filter(year <= emYear.x | plotGroup == "ATsurvey") %>%
annualRE <- errCompare %>% filter(year == emYear.x | plotGroup == "ATsurvey") %>%
  select(age1plusEM, age1plusOM, errSmryBio, year, model_run.x, iteration, scenario, HCR, recScen, plotGroup) %>%
  group_by(year,  scenario, HCR, recScen, plotGroup) %>%#iteration,
  summarize(meanYrRE = mean(errSmryBio),
            medYrRE = median(errSmryBio),
            hiYrRE = quantile(errSmryBio, probs = 0.975),
            loYrRE = quantile(errSmryBio, probs = 0.025),
            q1YrRE = quantile(errSmryBio, probs = 0.25),
            q3YrRE = quantile(errSmryBio, probs = 0.75),
            nErrs = n())

annualRE %>% filter(plotGroup != "non-convrg", HCR == "HCR2",
                    recScen %in% refScens) %>%
  mutate(recScen = factor(recScen, levels = c("ARRec", "MICERec",
                                              "PDOcyclRec", "PDOclimRec",
                                              "RegRec", "SSTRec"))) %>%
  ggplot(aes(x = year, fill = recScen, group = year)) +
  geom_boxplot(aes(ymin = loYrRE, lower = q1YrRE, middle = medYrRE, 
                   upper = q3YrRE, ymax = hiYrRE),
               stat = "identity") +
  facet_wrap(~recScen, ncol = 2) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_fill_manual(values = brewer.pal(6, "Set2")[c(1,3,2,4:6)]) +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  labs(x = "Year", y = "Relative Error (%)") 

# Terminal estimate relative error
termRE <- errCompare %>% filter(year == emYear.x | plotGroup == "ATsurvey") %>%
            select(age1plusEM, age1plusOM, errSmryBio, year, model_run.x, nameHCR,
                   iteration, scenario, HCR, recScen, plotGroup, recDevState) %>%
            mutate(closeYr = age1plusOM < 150000,
                   closeYrEst = age1plusEM < 150000,
                   collapseYr = age1plusOM < 50000,
                   collapseYrEst = age1plusEM < 50000,
                   recDevState = factor(recDevState, levels = c("high", "avg", "poor")))

# Relative error in all terminal years
termRE %>% group_by(model_run.x, iteration, scenario, HCR, recScen, plotGroup) %>%
  summarize(termRE = mean(errSmryBio)) %>% 
  filter(plotGroup != "non-convrg") %>%
  ggplot(aes(x = HCR, y = termRE, fill = HCR)) +
  geom_boxplot() +
  facet_wrap(~recScen) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Mean Relative Error of Terminal Assessment Year") +
  scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
  labs(caption = "Boxplot of the mean assessment relative error in the assessment year for Age1+ biomass.\nMeans over model assessment and year, levels of boxplot are 25th, 50th, and 75th quartiles, whiskers are 1.5IQR.") +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot")

# Relative error in terminal years by rec dev state
names(hcrLabels) <- c("HCR0", "HCR1" ,"HCR2", "HCR3", "HCR5", 
                      "HCR6", "HCR7", "HCR8", "HCR9")

bioStateLabs <- c("Biomass >150K mt", "Biomass <150K mt")
names(bioStateLabs) <- c(FALSE, TRUE)

termRE %>% filter(plotGroup != "non-convrg", recScen %in% refScens) %>%
  group_by(recDevState, closeYr, HCR) %>%#recScen,iteration,scenario,model_run.x, HCR,
  summarize(meanYrRE = mean(errSmryBio),
            medYrRE = median(errSmryBio),
            hiYrRE = quantile(errSmryBio, probs = 0.975),
            loYrRE = quantile(errSmryBio, probs = 0.025),
            q1YrRE = quantile(errSmryBio, probs = 0.25),
            q3YrRE = quantile(errSmryBio, probs = 0.75),
            nErrs = n()) %>%
  ggplot(aes(x = recDevState, fill = recDevState)) +
  geom_boxplot(aes(ymin = loYrRE, lower = q1YrRE, middle = medYrRE,
                   upper = q3YrRE, ymax = hiYrRE),
               stat = "identity") +
  # geom_boxplot(aes(y = errSmryBio), notch = TRUE) +
  facet_grid(closeYr ~ HCR,
             labeller = labeller(HCR = hcrLabels[-1], closeYr = bioStateLabs)) +
  # facet_wrap(~closeYr) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = rev(brewer.pal(n = 3, name = "BrBG"))) +
  labs(y = "Relative Error (%)", x = "Recruitment State") +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  theme(legend.position = "none")

# Relative error in terminal years by rec dev state
termRE %>% filter(plotGroup != "non-convrg", recScen %in% refScens) %>%
  group_by(year,  recDevState, closeYr) %>% #HCR,
  summarize(meanYrRE = mean(errSmryBio),
            medYrRE = median(errSmryBio),
            hiYrRE = quantile(errSmryBio, probs = 0.975),
            loYrRE = quantile(errSmryBio, probs = 0.025),
            q1YrRE = quantile(errSmryBio, probs = 0.25),
            q3YrRE = quantile(errSmryBio, probs = 0.75),
            nErrs = n()) %>% 
  ggplot(aes(x = as.factor(year), fill = recDevState)) +
  geom_boxplot(aes(ymin = loYrRE, lower = q1YrRE, middle = medYrRE, 
                   upper = q3YrRE, ymax = hiYrRE),
               stat = "identity") +
  facet_grid(rows = vars(recDevState), cols = vars(closeYr)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #labs(title = "Relative Error of Terminal Assessment Year") +
  #scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
  labs(x = "Year", y = "Relative Error (%)") +
  geom_hline(yintercept = 0, color = "red")

# Relative error in terminal years when biomass below closure threshold
termRE %>% filter(closeYr) %>%
  group_by(model_run.x, iteration, scenario, HCR, recScen, plotGroup) %>%
  summarize(termRE = mean(errSmryBio)) %>% 
  filter(plotGroup != "non-convrg") %>%
  ggplot(aes(x = HCR, y = termRE, fill = HCR)) +
  geom_boxplot() +
  # facet_wrap(~recScen) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Mean Relative Error of Terminal Assessment Year") +
  scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
  #labs(caption = "Boxplot of the mean assessment relative error in the assessment year for Age1+ biomass.\nMeans over model assessment and year, levels of boxplot are 25th, 50th, and 75th quartiles, whiskers are 1.5IQR.") +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot")

# Relative error in terminal years when biomass below collapse threshold
termRE %>% filter(collapseYr) %>%
  group_by(model_run.x, iteration, scenario, HCR, recScen, plotGroup) %>%
  summarize(termRE = mean(errSmryBio)) %>% 
  filter(plotGroup != "non-convrg") %>%
  ggplot(aes(x = HCR, y = termRE, fill = HCR)) +
  geom_boxplot() +
  # facet_wrap(~recScen) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Mean Relative Error of Terminal Assessment Year") +
  scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
  # labs(caption = "Boxplot of the mean assessment relative error in the assessment year for Age1+ biomass.\nMeans over model assessment and year, levels of boxplot are 25th, 50th, and 75th quartiles, whiskers are 1.5IQR.") +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot")

# Confusion matrix
confErr <- termRE %>% filter(plotGroup != "non-convrg") %>%
  group_by(year, iteration, scenario, nameHCR, HCR, recScen) %>%
  mutate(closeTruePos = isTRUE(closeYr) & isTRUE(closeYrEst),
         closeFalsePos = isFALSE(closeYr) & isTRUE(closeYrEst),
         closeTrueNeg = isFALSE(closeYr) & isFALSE(closeYrEst),
         closeFalseNeg = isTRUE(closeYr) & isFALSE(closeYrEst),
         collapseTruePos = isTRUE(collapseYr) & isTRUE(collapseYrEst),
         collapseFalsePos = isFALSE(collapseYr) & isTRUE(collapseYrEst),
         collapseTrueNeg = isFALSE(collapseYr) & isFALSE(collapseYrEst),
         collapseFalseNeg = isTRUE(collapseYr) & isFALSE(collapseYrEst),
         closePropFalsePos = closeFalsePos * (errSmryBio/100),
         closePropFalseNeg = closeFalseNeg *(errSmryBio/100),
         collapsePropFalsePos = collapseFalsePos * (errSmryBio/100),
         collapsePropFalseNeg = collapseFalseNeg *(errSmryBio/100)) %>%
  group_by(scenario, nameHCR, HCR, recScen) %>%
  summarize(closeTruePos = sum(closeTruePos),
            closeFalsePos = sum(closeFalsePos),
            closeTrueNeg = sum(closeTrueNeg),
            closeFalseNeg = sum(closeFalseNeg),
            collapseTruePos = sum(collapseTruePos),
            collapseFalsePos = sum(collapseFalsePos),
            collapseTrueNeg = sum(collapseTrueNeg),
            collapseFalseNeg = sum(collapseFalseNeg),
            closePropFalsePos = mean(closePropFalsePos)*100,
            closePropFalseNeg = mean(closePropFalseNeg)*100,
            collapsePropFalsePos = mean(collapsePropFalsePos)*100,
            collapsePropFalseNeg = mean(collapsePropFalseNeg)*100) %>%
  mutate(nAssess = closeTruePos + closeFalsePos + closeTrueNeg + closeFalseNeg, # change to just during closure
         nClose = closeTruePos + closeFalseNeg,
         nCollapse = collapseTruePos + collapseFalseNeg,
         closeErrRate = (closeFalsePos + closeFalseNeg)/nAssess,
         closeFalsePosRate = closeFalsePos/(nAssess-nClose),
         closeFalseNegRate = closeFalseNeg/nClose,
         collapseErrRate = (collapseFalsePos + collapseFalseNeg)/nAssess,
         collapseFalsePosRate = collapseFalsePos/(nAssess-nCollapse),
         collapseFalseNegRate = collapseFalseNeg/nCollapse,
         nameHCR = factor(nameHCR, levels = c("NoCat", "PFMCF018", "PFMCFSST", "ConstF",
                                              "Index", "Pikitch", "40-10", "DynPik", "Dyn40-10")),
         recScen = factor(recScen, levels = c("ARRec", "PDOcyclRec", 
                                              "MICERec", "PDOclimRec",
                                              "RegRec", "SSTRec")))

errLabs <- c("Missed Cutoff", "Missed Collapse", 
             "Mean Conditional Relative Error:\nCutoff (%)",
             "Mean Conditional Relative Error:\nCollapse (%)",
             "Total Cutoff Error", "Total Collapse Error")
names(errLabs) <- c("closeFalseNegRate", "collapseFalseNegRate", "closePropFalseNeg", 
                    "collapsePropFalseNeg", "closeErrRate", "collapseErrRate")


confErrXHCR <- confErr %>% filter(recScen %in% refScens) %>% 
  select(scenario, nameHCR, HCR, recScen, closeFalseNegRate, collapseFalseNegRate, 
         closePropFalseNeg, collapsePropFalseNeg,
         closeErrRate, collapseErrRate) %>%
  pivot_longer(c(closeFalseNegRate, collapseFalseNegRate, closePropFalseNeg, 
                 collapsePropFalseNeg, closeErrRate, collapseErrRate), 
               names_to = "errMetric", values_to = "vals") %>%
  ggplot(aes(x = nameHCR, y = vals, fill = recScen)) +
  geom_col(position = "dodge") +
  facet_wrap(~errMetric, scales = "free",
             labeller = labeller(errMetric = errLabs)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = brewer.pal(6, "Set2")) +
  labs(y = "Error Rate", x = "HCR", fill = "Recruitment\nScenario")

propErrXHCR <- confErr %>% filter(recScen %in% refScens) %>% 
  select(scenario, HCR, recScen, closePropFalsePos, closePropFalseNeg, 
         collapsePropFalsePos, collapsePropFalseNeg) %>%
  pivot_longer(c(closePropFalsePos, closePropFalseNeg, 
                 collapsePropFalsePos, collapsePropFalseNeg), 
               names_to = "errMetric", values_to = "vals") %>%
  ggplot(aes(x = HCR, y = vals*100, fill = recScen)) +
  geom_dotplot(binaxis = "y") +
  facet_wrap(~errMetric) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Mean Proportional Error (%)", x = "HCR")

confErrViolin <- confErr %>% filter(recScen %in% refScens) %>% 
  select(scenario, HCR, recScen, closeFalseNegRate, collapseFalseNegRate,
         closeErrRate, collapseErrRate) %>%
  pivot_longer(c(closeFalseNegRate, collapseFalseNegRate, closeErrRate, collapseErrRate), 
               names_to = "errMetric", values_to = "vals") %>%
  ggplot(aes(x = errMetric, y = vals)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = recScen)) +
  # facet_wrap(~errMetric) +
  theme_minimal() +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Error Rate", x = "HCR")

grid.arrange(confErrXHCR, confErrViolin)

# Plot errors in receiver operating characteristic space
confErr %>% filter(recScen %in% refScens) %>%
  ggplot() +
  geom_point(aes(x = closeFalsePosRate, y = 1-closeFalseNegRate, color = HCR)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_color_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
  ylim(0,1) +
  facet_wrap(~recScen)

confErr %>% filter(recScen %in% refScens) %>% 
  select(scenario, HCR, recScen, closeFalsePosRate, collapseFalsePosRate) %>%
  pivot_longer(c(closeFalsePosRate, collapseFalsePosRate), names_to = "errRate", values_to = "vals") %>%
  ggplot(aes(x = HCR, y = vals, fill = recScen)) +
  geom_dotplot(binaxis = "y") +
  facet_wrap(~errRate) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "False Closure Error Rate", x = "HCR")

confErr %>% filter(recScen %in% refScens) %>% 
  select(scenario, HCR, recScen, closeFalsePosRate, collapseFalsePosRate) %>%
  pivot_longer(c(closeFalsePosRate, collapseFalsePosRate), names_to = "errRate", values_to = "vals") %>%
  ggplot(aes(x = errRate, y = vals)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = recScen)) +
  # facet_wrap(~errRate) +
  theme_minimal() +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "False Closure Error Rate", x = "HCR")

confErr %>% filter(recScen %in% refScens) %>% 
  select(scenario, HCR, recScen, closeFalseNegRate, collapseFalseNegRate) %>%
  pivot_longer(c(closeFalseNegRate, collapseFalseNegRate), names_to = "errRate", values_to = "vals") %>%
  ggplot(aes(x = HCR, y = vals, fill = recScen)) +
  geom_dotplot(binaxis = "y") +
  facet_wrap(~errRate) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Missed Closure Error Rate", x = "HCR")

# Metrics in iterations with poor biomass -----------------------------

# sampleDat %>% filter(year > 2019, meanAge1Plus >= 150000, 
#                      recScen %in% refScens)
# # population typically recovers by 2025 in reference scenarios
# 
# poorBio <- smryOutputList$tsSmry %>% filter(model_run == omName, 
#                                             scenario %in% scenarios,
#                                             grepl("HCR0", scenario, fixed = TRUE), 
#                                             year >= 2030, Seas == 1) %>%
#               select(Bio_smry, rec_dev, year, iteration, scenario) %>%
#               mutate(didCollapse = Bio_smry < 50000,
#                      didClose = Bio_smry < 150000,
#                      HCR = sub(pattern = ".*Rec","", scenario),
#                      recScen = sub(pattern = "HCR.*","", scenario)) %>%
#               mutate(recScen = sub(pattern = ".*EM_","", recScen))
# 
# poorBioSamps <- poorBio %>%
#   group_by(recScen, iteration) %>% 
#   summarize(NyrsClose = sum(didClose),
#             NyrsCollapse = sum(didCollapse)) %>% 
#   filter(NyrsClose > 0) %>% 
#   select(recScen, iteration)
# 
# 
# # first summarize number of iterations with closures and collapses after 10 yrs
# # ID iteration/recScen combos with neg outcomes
# dim(unique(poorBio[poorBio$didClose, c("iteration", "recScen")]))
# 
# unique(poorBio[poorBio$didClose, c("iteration", "recScen")]) %>% 
#   group_by(recScen) %>%
#   summarize(itsN = n())
# 
# unique(poorBio[poorBio$didCollapse, c("iteration", "recScen")]) %>% 
#   group_by(recScen) %>%
#   summarize(itsN = n())
# 
# # Recalculate metrics using only iterations with >5 poor recruitment years
# poorBioOutList <- readRDS(file.path(mseDir, 
#                                     "serverRegARPDOcyclPDOclimSSTMICE_allHCRs_results.RDS"))
# 
# poorBioOutList$dqSmry <- poorBioOutList$dqSmry %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen)) %>%
#   right_join(y = poorBioSamps, by = c("recScen", "iteration"))
# 
# poorBioOutList$sclSmry <- poorBioOutList$sclSmry %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen)) %>%
#   right_join(y = poorBioSamps, by = c("recScen", "iteration"))
# 
# poorBioOutList$tsSmry <- poorBioOutList$tsSmry %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen)) %>%
#   right_join(y = poorBioSamps, by = c("recScen", "iteration"))
# 
# poorBioPerfList <- CalcPerformance(poorBioOutList)
# 
# poorBioMetrics <- poorBioPerfList$perfomanceMetrics %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen))
# 
# # Clear up some memory
# poorBioOutList$lenComp <- NULL
# poorBioOutList$ageComp <- NULL
# 
# # get terminal estimates of these values for timeseries plots
# poorBioTermTS <- CalcTermTS(poorBioOutList, termYr = termYr) %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen))
# 
# annualRelBioCatPoorBio <- poorBioTermTS %>% filter(year > 2019, year != 2070,
#                                                    model_run == omName) %>%
#   left_join(y = relDenoms,
#             by = c("recScen", "iteration")) %>%
#   mutate(relAnnBioMax = Bio_smry/maxMeanBio,
#          relAnnCatMax = totCatch/maxMeanCat)
# 
# # plot annual relative biomass metrics
# relBioPoorBio <- annualRelBioCatPoorBio %>% filter(recScen %in% refScens) %>%
#   group_by(HCR) %>%
#   summarize(meanRelBio = mean(relAnnBioMax),
#             medRelBio = median(relAnnBioMax),
#             hiRelBio = quantile(relAnnBioMax, probs = 0.975),
#             loRelBio = quantile(relAnnBioMax, probs = 0.025),
#             q1RelBio = quantile(relAnnBioMax, probs = 0.25),
#             q3RelBio = quantile(relAnnBioMax, probs = 0.75),
#             nRelBio = n()) %>% 
#   ggplot(aes(x = HCR, fill = HCR)) +
#   geom_boxplot(aes(ymin = loRelBio, lower = q1RelBio, middle = medRelBio, 
#                    upper = q3RelBio, ymax = hiRelBio),
#                stat = "identity") +
#   # ggplot(aes(x = HCR, y = relAnnBioMax)) +
#   # # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
#   # geom_boxplot(aes(fill = HCR), outlier.shape = NA) +
#   # coord_cartesian(ylim = c(0,2.5)) +
#   # geom_jitter(aes(color = recScen, alpha = 0.03)) +
#   #facet_wrap(~recScen, scales = "free") +
#   theme_minimal() +
#   scale_fill_manual(values = hcrPal, labels = hcrLabels) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(y = "Annual Age1+ Biomass:Max in Closure Iters", x = "HCR")
# 
# grid.arrange(relBioAll, relBioPoorBio)
# 
# relCatPoorBio <- annualRelBioCatPoorBio %>% filter(HCR != "HCR0",
#                                                    recScen %in% refScens) %>%
#   ggplot(aes(x = HCR, y = relAnnCatMax)) +
#   # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
#   geom_boxplot(aes(fill = HCR), outlier.shape = NA) +
#   coord_cartesian(ylim = c(0,2.5)) +
#   # geom_jitter(aes(color = recScen, alpha = 0.03)) +
#   # facet_wrap(~recScen, scales = "free") +
#   theme_minimal() +
#   scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(y = "Annual Catch:Max in Closure Iters", x = "HCR")
# 
# grid.arrange(relCatAll, relCatPoorBio)
# 
# sdCatPoorBio <- poorBioMetrics %>%  filter(HCR != "HCR0", 
#                                            recScen %in% refScens) %>%
#   ggplot(aes(x = HCR, y = sdCatch)) +
#   geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
#   #facet_wrap(~recScen, scales = "free") + 
#   theme_minimal() +
#   scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(y = "Catch SD (mt)", x = "HCR")
# 
# 
# termTS %>% filter(model_run == omName,  year > 2019,
#                   # look at poor biomass iterations
#                   # recScen == "PDOclimRec", iteration %in% c(4, 10, 15, 42, 68, 83,
#                   #                                           14, 21, 32, 41, 65, 88)) %>%
#                   recScen == "PDOcyclRec", iteration %in% c(56, 49, 68, 95, 24, 28,
#                                                             # compare to non-closed iterations
#                                                             14, 22, 35, 45, 65, 64)) %>%
#   ggplot(aes(x = year, y = log(Bio_smry), color = HCR)) +
#   geom_hline(yintercept = c(log(150000),log(50000))) +
#   geom_line() +
#   facet_wrap(~iteration) +
#   geom_rug(data = termTS %>% filter(model_run == omName, year > 2019,
#                                     # recScen == "PDOclimRec", iteration %in% c(4, 10, 15, 42, 68, 83,
#                                     #                                           14, 21, 32, 41, 65, 88),
#                                     recScen == "PDOcyclRec", iteration %in% c(56, 49, 68, 95, 24, 28,
#                                                                               # compare to non-closed iterations
#                                                                               14, 22, 35, 45, 65, 64),
#                                     rec_dev < -1.25),
#            mapping = aes(x = year), sides = "b") #+
#   # geom_line(aes(x = year, y = rec_dev*2e5 + 4e6)) +
#   # geom_hline(yintercept = 4e6, color = "grey")
# 
# 
# # Metrics in iterations with poor recruitment -----------------------------
# 
# # Find how many iterations don't have projected rec_devs < -1.25
# poorRec <- termTS %>% filter(model_run == omName, HCR == "HCR0",
#                              year > 2019, 
#                              rec_dev < -1.25) %>%
#               group_by(recScen, iteration) %>% 
#               summarize(NyrsPoorRec = n())
# poorRec %>% filter(NyrsPoorRec > 5) %>%
#   group_by(recScen) %>%
#   summarize(Nits = n())
# 
# # Recalculate metrics using only iterations with >5 poor recruitment years
# poorRecOutList <- readRDS(file.path(mseDir, 
#                                     "serverRegARPDOcyclPDOclimSSTMICE_allHCRs_results.RDS"))
# poorRecSamps <- poorRec %>% filter(NyrsPoorRec > 5,
#                                    recScen %in% refScens) %>% 
#                   select(recScen, iteration)
# 
# poorRecOutList$dqSmry <- poorRecOutList$dqSmry %>%
#                           mutate(HCR = sub(pattern = ".*Rec","", scenario),
#                                  recScen = sub(pattern = "HCR.*","", scenario)) %>%
#                           mutate(recScen = sub(pattern = ".*EM_","", recScen)) %>%
#                           right_join(y = poorRecSamps, by = c("recScen", "iteration"))
# 
# poorRecOutList$sclSmry <- poorRecOutList$sclSmry %>%
#                             mutate(HCR = sub(pattern = ".*Rec","", scenario),
#                                    recScen = sub(pattern = "HCR.*","", scenario)) %>%
#                             mutate(recScen = sub(pattern = ".*EM_","", recScen)) %>%
#                             right_join(y = poorRecSamps, by = c("recScen", "iteration"))
# 
# poorRecOutList$tsSmry <- poorRecOutList$tsSmry %>%
#                           mutate(HCR = sub(pattern = ".*Rec","", scenario),
#                                  recScen = sub(pattern = "HCR.*","", scenario)) %>%
#                           mutate(recScen = sub(pattern = ".*EM_","", recScen)) %>%
#                           right_join(y = poorRecSamps, by = c("recScen", "iteration"))
# 
# poorRecPerfList <- CalcPerformance(poorRecOutList)
# 
# poorRecMetrics <- poorRecPerfList$perfomanceMetrics %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen))
# 
# # Clear up some memory
# poorRecOutList$lenComp <- NULL
# poorRecOutList$ageComp <- NULL
# 
# # get terminal estimates of these values for timeseries plots
# poorRecTermTS <- CalcTermTS(poorRecOutList, termYr = termYr) %>%
#   mutate(HCR = sub(pattern = ".*Rec","", scenario),
#          recScen = sub(pattern = "HCR.*","", scenario)) %>%
#   mutate(recScen = sub(pattern = ".*EM_","", recScen))
# 
# annualRelBioCatPoorRec <- poorRecTermTS %>% filter(year > 2019, year != 2070,
#                                      model_run == omName) %>%
#   left_join(y = relDenoms,
#             by = c("recScen", "iteration")) %>%
#   mutate(relAnnBioMax = Bio_smry/maxMeanBio,
#          relAnnCatMax = totCatch/maxMeanCat)
# 
# # plot annual relative biomass metrics
# relBioPoorRec <- annualRelBioCatPoorRec %>% filter(recScen %in% refScens) %>%
#   group_by(HCR) %>%
#   summarize(meanRelBio = mean(relAnnBioMax),
#             medRelBio = median(relAnnBioMax),
#             hiRelBio = quantile(relAnnBioMax, probs = 0.975),
#             loRelBio = quantile(relAnnBioMax, probs = 0.025),
#             q1RelBio = quantile(relAnnBioMax, probs = 0.25),
#             q3RelBio = quantile(relAnnBioMax, probs = 0.75),
#             nRelBio = n()) %>% 
#   ggplot(aes(x = HCR, fill = HCR)) +
#   geom_boxplot(aes(ymin = loRelBio, lower = q1RelBio, middle = medRelBio, 
#                    upper = q3RelBio, ymax = hiRelBio),
#                stat = "identity") +
#   # ggplot(aes(x = HCR, y = relAnnBioMax)) +
#   # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
#   # geom_jitter(aes(color = recScen, alpha = 0.03)) +
#   #facet_wrap(~recScen, scales = "free") +
#   theme_minimal() +
#   scale_fill_manual(values = hcrPal, labels = hcrLabels) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(y = "Annual Age1+ Biomass:Max in Poor Recruitment Iters", x = "HCR")
# 
# grid.arrange(relBioAll, relBioPoorRec,
#              relBioClose, relBioColl)
# grid.arrange(relBioAll, relBioPoorRec, relBioPoorBio, nrow = 2)
# 
# relCatPoorRec <- annualRelBioCatPoorRec %>% filter(HCR != "HCR0",
#                                                    recScen %in% refScens) %>%
#   group_by(HCR) %>%
#   summarize(meanRelCat = mean(relAnnCatMax),
#             medRelCat = median(relAnnCatMax),
#             hiRelCat = quantile(relAnnCatMax, probs = 0.975),
#             loRelCat = quantile(relAnnCatMax, probs = 0.025),
#             q1RelCat = quantile(relAnnCatMax, probs = 0.25),
#             q3RelCat = quantile(relAnnCatMax, probs = 0.75),
#             nRelCat = n()) %>% 
#   ggplot(aes(x = HCR, fill = HCR)) +
#   geom_boxplot(aes(ymin = loRelCat, lower = q1RelCat, middle = medRelCat, 
#                    upper = q3RelCat, ymax = hiRelCat),
#                stat = "identity") +
#   # ggplot(aes(x = HCR, y = relAnnCatMax)) +
#   # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
#   # geom_jitter(aes(color = recScen, alpha = 0.03)) +
#   # facet_wrap(~recScen, scales = "free") +
#   theme_minimal() +
#   scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(y = "Annual Catch:Max in Poor Recruitment Iters", x = "HCR")
# 
# sdCatPoorRec <- poorRecMetrics %>%  filter(HCR != "HCR0", 
#                                            recScen %in% refScens,
#                                            !is.na(sdCatch)) %>% # NOTE: has all iterations, but non-poor rec its are all NA
#   group_by(HCR) %>%
#   summarize(meanRelCatSD = mean(sdCatch),
#             medRelCatSD = median(sdCatch),
#             hiRelCatSD = quantile(sdCatch, probs = 0.975),
#             loRelCatSD = quantile(sdCatch, probs = 0.025),
#             q1RelCatSD = quantile(sdCatch, probs = 0.25),
#             q3RelCatSD = quantile(sdCatch, probs = 0.75),
#             nRelCatSD = n()) %>% 
#   ggplot(aes(x = HCR, fill = HCR)) +
#   geom_boxplot(aes(ymin = loRelCatSD, lower = q1RelCatSD, middle = medRelCatSD, 
#                    upper = q3RelCatSD, ymax = hiRelCatSD),
#                stat = "identity") +
#   # ggplot(aes(x = HCR, y = sdCatch)) +
#   # geom_violin(aes(fill = HCR), draw_quantiles = c(0.1, 0.5, 0.9)) +
#   #facet_wrap(~recScen, scales = "free") + 
#   theme_minimal() +
#   scale_fill_manual(values = hcrPal[-1], labels = hcrLabels[-1]) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(y = "Catch SD (mt)", x = "HCR")
# 
# grid.arrange(relCatAll, sdCatAll,
#              relCatPoorRec, sdCatPoorRec)
# 
# annualRelBioCat %>% filter(recScen %in% refScens) %>%
#   group_by(HCR) %>%
#   summarize(meanRelCat = mean(relAnnCatMax),
#             medRelCat = median(relAnnCatMax),
#             high90RelCat = quantile(relAnnCatMax, probs = 0.9),
#             low10RelCat = quantile(relAnnCatMax, probs = 0.1),
#             meanRelBio = mean(relAnnBioMax),
#             medRelBio = median(relAnnBioMax),
#             high90RelBio = quantile(relAnnBioMax, probs = 0.9),
#             low10RelBio = quantile(relAnnBioMax, probs = 0.1))
# 
# annualRelBioCatPoorRec %>% filter(recScen %in% refScens) %>%
#   group_by(HCR) %>%
#   summarize(meanRelCat = mean(relAnnCatMax),
#             medRelCat = median(relAnnCatMax),
#             high90RelCat = quantile(relAnnCatMax, probs = 0.9),
#             low10RelCat = quantile(relAnnCatMax, probs = 0.1),
#             meanRelBio = mean(relAnnBioMax),
#             medRelBio = median(relAnnBioMax),
#             high90RelBio = quantile(relAnnBioMax, probs = 0.9),
#             low10RelBio = quantile(relAnnBioMax, probs = 0.1))
# 
# annualRelBioCat %>% filter(recScen %in% refScens,
#                            Bio_smry < 150000) %>%
#   group_by(HCR) %>%
#   summarize(meanRelBio = mean(relAnnBioMax),
#             medRelBio = median(relAnnBioMax),
#             high90RelBio = quantile(relAnnBioMax, probs = 0.9),
#             low10RelBio = quantile(relAnnBioMax, probs = 0.1))
# 
# annualRelBioCat %>% filter(recScen %in% refScens,
#                            Bio_smry < 50000) %>%
#   group_by(HCR) %>%
#   summarize(meanRelBio = mean(relAnnBioMax),
#             medRelBio = median(relAnnBioMax),
#             high90RelBio = quantile(relAnnBioMax, probs = 0.9),
#             low10RelBio = quantile(relAnnBioMax, probs = 0.1))
# 
# # Compare final simulation years to initial biomass
# bio2019 <- smryOutputList$tsSmry %>% filter(model_run == omName, 
#                                             grepl("HCR0", scenario, fixed = TRUE), 
#                                             year == 2019, Seas == 1) %>% 
#                     pull(Bio_smry)
# bio2019 <- unique(bio2019)
# 
# worseBio2019 <- smryOutputList$tsSmry %>% filter(model_run == omName, 
#                                  #grepl("HCR0", scenario, fixed = TRUE), 
#                                  year > 2060, Seas == 1, Bio_smry < bio2019) %>%
#                   select(Bio_smry, rec_dev, year, iteration, scenario) 
# unique(worseBio2019[, c("iteration", "scenario")])
# # Only ARRec scenario has unrecovered pops at end of simulation
# 
