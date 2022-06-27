library(r4ss)
library(ss3sim)
library(dplyr)
source("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/R/SSMethod.TA1.8mod.R")
margCompsServit1_2041 <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/scenarioRuns/start1981HCR6serverRuns/1/margCompsOMfixedSelexEM_EM_2041",
                                   covar = FALSE)
SSplotComps(replist = margCompsServit1_2041, kind = "AGE")

test2 <- SSMethod.TA1.8mod(fit = margCompsServit1_2041, type = "len", fleet = 4, plotit = TRUE)
test2[[2]]
margCompsServit1_2041$lendbase

barplot(margCompsServit1_2041$lendbase$Exp, subset)


margCompsServit1_2041$agedbase %>% group_by(Fleet, Yr) %>% 
  summarize(mean_age_obs = sum(Obs * Bin), mean_age_exp = sum(Exp * Bin))

margCompsServit1_2041$timeseries
margCompsServit1_2041$sprseries

constCompsServit1_2044 <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/testConvrgEM2001fixedParms_RandRec_HCR1/1/testConvrgShortEM_EM_2044",
                                   covar = FALSE)
SSplotComps(replist = constCompsServit1_2044, kind = "AGE")
constCompsServit1_2044$lendbase

# see if modification of ss3sim get-results works
testGetTS <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/testConvrgEM2001fixedParms_RandRec_HCR3/1/testConvrgShortEM_EM_2020",
                                   covar = FALSE)
originalTS <- get_results_timeseries(testGetTS)

devtools::load_all(path = "C:/Users/r.wildermuth/Documents/ss3sim")
newTS <- get_results_timeseries(testGetTS)

# Pull summaries from test run --------------------------------------------

dir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios"
scenario <- "testConvrgEM2005fixedParms_RandRec_HCR1"
termYr <- 2068


testBioRecOM <- inner_join(dqSumry, smrySPRseries, by = c("model_run", "iteration",
                                                         "year" = "Yr")) %>% 
                  mutate(checkBio = Value.SSB == SSB,
                         checkRec = Value.Recr == Recruits)
all(testBioRecOM$checkBio)
all(testBioRecOM$checkRec)

# these don't match, estimates come from FORE Era?
testBioRecEM <- dqSumry %>% filter(model_run != "start2001_OM") %>%
                  mutate(emYear = as.numeric(regmatches(model_run,
                                                        gregexpr("[[:digit:]]+", 
                                                                 model_run)))) %>%
                  filter(emYear == year) %>%
                  inner_join(y = smrySPRseries, by = c("iteration", "emYear" = "Yr"))

testCatch <- tsSumry %>% mutate(totCatch = retainB_1 + retainB_2 + retainB_3) %>% 
                group_by(year, model_run, iteration, scenario) %>%
                # summarize total catch within year
                dplyr::summarize(totCatch = sum(totCatch)) %>% 
                mutate(emYear = as.numeric(regmatches(model_run,
                                                      gregexpr("[[:digit:]]+", 
                                                               model_run))))

testTermCatch <- smryCatch %>% group_by(Yr, model_run, iteration) %>%
                    # summarize total catch within year
                    dplyr::summarize(obsCat = sum(Obs),
                                     expCat = sum(Exp))

testTermCatchOM <- testTermCatch %>% filter(grepl("start2001_OM", model_run, fixed = TRUE))

testCatchOM <- testCatch %>% filter(model_run == "start2001_OM") %>%
                  inner_join(y = testTermCatchOM, by = c("iteration", "year" = "Yr")) %>%
                  mutate(checkCat = round(totCatch, digits = 2) == round(expCat, digits = 2))
                
testTermCatchEM <- testTermCatch %>% filter(!grepl("start2001_OM", model_run, fixed = TRUE))

testCatchEM <- testCatch %>% filter(model_run != "start2001_OM") %>%
  inner_join(y = testTermCatchEM, by = c("iteration", "year" = "Yr")) %>%
  mutate(checkCat = round(totCatch, digits = 2) == round(expCat, digits = 2))

smryDQs %>% filter(Label == "SmryBio_unfished") # these are forecasts of year following EM year
sclSumry %>% filter(iteration == 1)

source("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/R/GetSumryOutput.R")

testConEM2001out <- GetSumryOutput(dirSSMSE = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios",
                                   scenarios = c("testConvrgEM2001fixedParms_RandRec_HCR1",
                                                "testConvrgEM2001fixedParms_RandRec_HCR3"))
str(testConEM2001out)
unique(testConEM2001out$sclSmry$scenario)

