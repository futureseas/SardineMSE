# Set up EM folder from SSMSE run to use perfect info from OM folder to test fit

library(r4ss)
library(tidyverse)

OM_dir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_MidSteepMidNsamp_RandRecHCR2/1/constGrowthSteepness0dot6_OM_OM"
EM_dir <- "C:/Users/r.wildermuth/Desktop/perfectDatTest_EM_2037"

datOM <- SS_readdat(file.path(OM_dir, "data.ss_new"),
                    verbose = FALSE,
                    section = 2)

datEM <- SS_readdat(file.path(EM_dir, "init_dat.ss"),
                    verbose = FALSE,
                    section = 1)

# Check indexing
unique(datEM$catch$year)
unique(datEM$catch$fleet)
unique(datEM$CPUE$year)
unique(datEM$CPUE$index)
unique(datEM$lencomp$Yr)
unique(datEM$lencomp$FltSvy)
unique(datEM$agecomp$Yr)
unique(datEM$agecomp$FltSvy)


unique(datOM$catch$year)
unique(datOM$catch$fleet)
unique(datOM$CPUE$year)
unique(datOM$CPUE$index)
unique(datOM$lencomp$Yr)
unique(datOM$lencomp$FltSvy) # keep -3 FltSvy for seas 10
unique(datOM$agecomp$Yr)
unique(datOM$agecomp$FltSvy)

datEM$catch <- datOM$catch %>% filter(year <= 2037)
datEM$CPUE <- datOM$CPUE %>% filter(year <= 2037) %>%
                mutate(index = abs(index))
datEM$lencomp <- datOM$lencomp %>% filter(Yr <= 2037) %>%
                    mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                                              TRUE ~ abs(FltSvy)))
datEM$agecomp <- datOM$agecomp %>% filter(Yr <= 2037) %>%
                    mutate(FltSvy = abs(FltSvy))

SS_writedat(datlist = datEM,
            outfile = file.path(file.path(EM_dir, "init_dat.ss")),
            overwrite = TRUE,
            verbose = FALSE)
### Run SS in EM_dir ###
outEM <- SS_output(EM_dir)
outOM <- SS_output(OM_dir)

perfComp <- SSsummarize(list(OM = outOM, EM2037 = outEM))
SSplotComparisons(perfComp)

perfComp$pars$relErr <- round((perfComp$pars$EM2037 - perfComp$pars$OM)/perfComp$pars$OM * 100, digits = 3)

SS_plots(outEM)

# Changed bias adjustment final year in control file ----------------------

EMnewBias_dir <- "C:/Users/r.wildermuth/Desktop/perfectDatTest_EMnewBiasEnd_2037"

outEMnewBias <- SS_output(EMnewBias_dir)

newBiasComp <- SSsummarize(list(OM = outOM, EM2037newBias = outEMnewBias))
SSplotComparisons(newBiasComp, plot = TRUE)

newBiasComp$pars$relErr <- round((newBiasComp$pars$EM2037newBias - newBiasComp$pars$OM)/newBiasComp$pars$OM * 100, digits = 3)

SS_plots(outEMnewBias)


# Try for initial 2019 assessment -----------------------------------------

EM_dir <- "C:/Users/r.wildermuth/Desktop/perfectDatTest_EM_init"

datEM <- SS_readdat(file.path(EM_dir, "init_dat.ss"),
                    verbose = FALSE,
                    section = 1)

datEM$catch <- datOM$catch %>% filter(year <= 2019)
datEM$CPUE <- datOM$CPUE %>% filter(year <= 2019) %>%
  mutate(index = abs(index))
datEM$lencomp <- datOM$lencomp %>% filter(Yr <= 2019) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
datEM$agecomp <- datOM$agecomp %>% filter(Yr <= 2019) %>%
  mutate(FltSvy = abs(FltSvy))

SS_writedat(datlist = datEM,
            outfile = file.path(file.path(EM_dir, "init_dat.ss")),
            overwrite = TRUE,
            verbose = FALSE)

### Run SS in EM_dir ###
outEM <- SS_output(EM_dir)

perfComp <- SSsummarize(list(OM = outOM, EM2037 = outEM))
SSplotComparisons(perfComp)


# Compare reference EM and OM folders -------------------------------------

OM_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthSteepness0dot6_OMnohess"
EM_dir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowthSteepness0dot6_EM"

outEM <- SS_output(EM_dir)
outOM <- SS_output(OM_dir)

perfComp <- SSsummarize(list(OM = outOM, EMref = outEM))
SSplotComparisons(perfComp)

# copy in simulated data from the SSMSE OM run
OM_SSMSEdir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_MidSteepMidNsamp_RandRecHCR2/1/constGrowthSteepness0dot6_OM_OM"
runDatOM <- SS_readdat(file.path(OM_SSMSEdir, "data.ss_new"),
                    verbose = FALSE,
                    section = 2)

datOM <- SS_readdat(file.path(OM_dir, "data.ss"),
                       verbose = FALSE,
                       section = 1)

newCatch <- runDatOM$catch %>% filter(year == 2020)
newCPUE <- runDatOM$CPUE %>% filter(year == 2020) %>%
  mutate(index = abs(index))
newLencomp <- runDatOM$lencomp %>% filter(Yr == 2020) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
newAgecomp <- runDatOM$agecomp %>% filter(Yr == 2020) %>%
  mutate(FltSvy = abs(FltSvy))

datOM$catch <- rbind(datOM$catch, newCatch)
datOM$CPUE <- rbind(datOM$CPUE, newCPUE)
datOM$lencomp <- rbind(datOM$lencomp, newLencomp)
datOM$agecomp <- rbind(datOM$agecomp, newAgecomp)

SS_writedat(datlist = datOM,
            outfile = file.path(file.path(OM_dir, "data.ss")),
            overwrite = TRUE,
            verbose = FALSE)
### Ran OM with no hess to update to 2020 data ###

# Now populate EM with 2020 OM data
EM_dir <- "C:/Users/r.wildermuth/Desktop/perfectDatTest_EM_init"

datEM <- SS_readdat(file.path(EM_dir, "init_dat.ss"),
                    verbose = FALSE,
                    section = 1)

datEM$catch <- datOM$catch %>% filter(year <= 2020)
datEM$CPUE <- datOM$CPUE %>% filter(year <= 2020) %>%
  mutate(index = abs(index))
datEM$lencomp <- datOM$lencomp %>% filter(Yr <= 2020) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
datEM$agecomp <- datOM$agecomp %>% filter(Yr <= 2020) %>%
  mutate(FltSvy = abs(FltSvy))

SS_writedat(datlist = datEM,
            outfile = file.path(file.path(EM_dir, "init_dat.ss")),
            overwrite = TRUE,
            verbose = FALSE)

### Run SS for EM_init model###

outEM <- SS_output(EM_dir)
outOM <- SS_output(OM_dir)

perfComp <- SSsummarize(list(OM = outOM, EM2020 = outEM))
SSplotComparisons(perfComp)

# Compare to if using the reference EM model with updated 2020 data
EMref_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthSteepness0dot6_EM_2020"

datEM2020 <- SS_readdat(file.path(EMref_dir, "data.ss"),
                    verbose = FALSE)

datEM2020$catch <- rbind(datEM2020$catch, newCatch)
datEM2020$CPUE <- rbind(datEM2020$CPUE, newCPUE)
datEM2020$lencomp <- rbind(datEM2020$lencomp, newLencomp)
datEM2020$agecomp <- rbind(datEM2020$agecomp, newAgecomp)

SS_writedat(datlist = datEM2020,
            outfile = file.path(file.path(EMref_dir, "init_dat.ss")),
            overwrite = TRUE,
            verbose = FALSE)

### Run SS to get perfect information fit ###
outEM2020 <- SS_output(EMref_dir)

perfComp <- SSsummarize(list(OM = outOM, 
                             EM2020 = outEM,
                             EM2020perf = outEM2020))
SSplotComparisons(perfComp)

########################################################################

OMnohess2019_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthMidSteep_OMnohess_2019"

datOM_2019 <- SS_readdat(file.path(OMnohess2019_dir, "data.ss_new"),
                    verbose = FALSE,
                    section = 2)

EMssmse2019_dir <- "C:/Users/r.wildermuth/Desktop/perfectDatTest_EM_2019"

datEMssmse2019 <- SS_readdat(file.path(EMssmse2019_dir, "init_dat.ss"),
                    verbose = FALSE,
                    section = 1)

datEMssmse2019$catch <- datOM_2019$catch %>% filter(year <= 2019)
datEMssmse2019$CPUE <- datOM_2019$CPUE %>% filter(year <= 2019) %>%
  mutate(index = abs(index))
datEMssmse2019$lencomp <- datOM_2019$lencomp %>% filter(Yr <= 2019) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
datEMssmse2019$agecomp <- datOM_2019$agecomp %>% filter(Yr <= 2019) %>%
  mutate(FltSvy = abs(FltSvy))

SS_writedat(datlist = datEMssmse2019,
            outfile = file.path(file.path(EMssmse2019_dir, "init_dat.ss")),
            overwrite = TRUE,
            verbose = FALSE)

### Run EMssmse2019 and EMref2019 SS models
### Copy directories to make 2020 folders
### update OMnohess2020 to generate bootstrap values, and end year in 2020
### copy simulated data from SSMSE OM run and include simulated rec_dev in par file
OM_SSMSEdir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrow2001OM_MidSteepMidNsamp_RandRecHCR2/1/constGrowthSteepness0dot6_OM_OM"
DatssmseOM <- SS_readdat(file.path(OM_SSMSEdir, "data.ss_new"),
                       verbose = FALSE,
                       section = 2)

OMnohess2020_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthMidSteep_OMnohess_2020"

datOM_2020 <- SS_readdat(file.path(OMnohess2020_dir, "data.ss"),
                    verbose = FALSE,
                    section = 1)

newCatch <- DatssmseOM$catch %>% filter(year == 2020)
newCPUE <- DatssmseOM$CPUE %>% filter(year == 2020) %>%
  mutate(index = abs(index))
newLencomp <- DatssmseOM$lencomp %>% filter(Yr == 2020) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
newAgecomp <- DatssmseOM$agecomp %>% filter(Yr == 2020) %>%
  mutate(FltSvy = abs(FltSvy))

datOM_2020$catch <- rbind(datOM_2020$catch, newCatch)
datOM_2020$CPUE <- rbind(datOM_2020$CPUE, newCPUE)
datOM_2020$lencomp <- rbind(datOM_2020$lencomp, newLencomp)
datOM_2020$agecomp <- rbind(datOM_2020$agecomp, newAgecomp)

SS_writedat(datlist = datOM_2020,
            outfile = file.path(file.path(OMnohess2020_dir, "data.ss")),
            overwrite = TRUE,
            verbose = FALSE)
### Run OMnohess2020 with -nohess
### input expected values of OMnohess2020 for 2020 into EMssmse2020 and EMref2020
# SSMSE run EM
datExpOM_2020 <- SS_readdat(file.path(OMnohess2020_dir, "data.ss_new"),
                         verbose = FALSE,
                         section = 2)

EMssmse2020_dir <- "C:/Users/r.wildermuth/Desktop/perfectDatTest_EM_2020"

datEMssmse2020 <- SS_readdat(file.path(EMssmse2020_dir, "init_dat.ss"),
                    verbose = FALSE,
                    section = 1)

datEMssmse2020$catch <- datExpOM_2020$catch %>% filter(year <= 2020)
datEMssmse2020$CPUE <- datExpOM_2020$CPUE %>% filter(year <= 2020) %>%
  mutate(index = abs(index))
datEMssmse2020$lencomp <- datExpOM_2020$lencomp %>% filter(Yr <= 2020) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
datEMssmse2020$agecomp <- datExpOM_2020$agecomp %>% filter(Yr <= 2020) %>%
  mutate(FltSvy = abs(FltSvy))

SS_writedat(datlist = datEMssmse2020,
            outfile = file.path(file.path(EMssmse2020_dir, "init_dat.ss")),
            overwrite = TRUE,
            verbose = FALSE)

# Reference EM
EMref2019_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthMidSteep_EM_2019"
EMref2020_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthMidSteep_EM_2020"

datEMref2020 <- SS_readdat(file.path(EMref2020_dir, "data.ss"),
                             verbose = FALSE,
                             section = 1)

om2020Catch <- datExpOM_2020$catch %>% filter(year == 2020)
om2020CPUE <- datExpOM_2020$CPUE %>% filter(year == 2020) %>%
  mutate(index = abs(index))
om2020Lencomp <- datExpOM_2020$lencomp %>% filter(Yr == 2020) %>%
  mutate(FltSvy = case_when(FltSvy == -3 & Seas == 10 ~ FltSvy,
                            TRUE ~ abs(FltSvy)))
om2020Agecomp <- datExpOM_2020$agecomp %>% filter(Yr == 2020) %>%
  mutate(FltSvy = abs(FltSvy))

datEMref2020$catch <- rbind(datEMref2020$catch, om2020Catch)
datEMref2020$CPUE <- rbind(datEMref2020$CPUE, om2020CPUE)
datEMref2020$lencomp <- rbind(datEMref2020$lencomp, om2020Lencomp)
datEMref2020$agecomp <- rbind(datEMref2020$agecomp, om2020Agecomp)

SS_writedat(datlist = datEMref2020,
            outfile = file.path(file.path(EMref2020_dir, "data.ss")),
            overwrite = TRUE,
            verbose = FALSE)
### Run EMssmse2020 and EMref2020 SS models in their folders

# plot comparisons
outOMnohess2019 <- SS_output(OMnohess2019_dir)
outEMssmse2019 <- SS_output(EMssmse2019_dir)
outEMref2019 <- SS_output(EMref2019_dir)

comp2019 <- SSsummarize(list(OM2019 = outOMnohess2019, 
                                   EMssmse2019 = outEMssmse2019,
                                   EMref2019 = outEMref2019))
SSplotComparisons(comp2019)

outOMnohess2020 <- SS_output(OMnohess2020_dir)
outEMssmse2020 <- SS_output(EMssmse2020_dir)
outEMref2020 <- SS_output(EMref2020_dir)

compTandem2020 <- SSsummarize(list(OMtandem2020 = outOMnohess2020, 
                             EMssmse2020 = outEMssmse2020,
                             EMref2020 = outEMref2020))
SSplotComparisons(compTandem2020)

### copy EMssmse2020 and EMref2020 folders, change last year of main rec_devs, and rerun estimation
EMrefrecmain2020_dir <- "C:/Users/r.wildermuth/Desktop/constGrowthMidSteep_EMrecmain_2020"
outEMrefrecmain2020 <- SS_output(EMrefrecmain2020_dir)

compRecMain2020 <- SSsummarize(list(OMtandem2020 = outOMnohess2020, 
                                   EMref2020 = outEMref2020,
                                   EMrefrecmain2020 = outEMrefrecmain2020))
SSplotComparisons(compRecMain2020)
