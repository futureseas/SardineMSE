#-------------------------------------------
options(max.print = 1000, device = 'windows')
library(plyr)
library(r4ss)
library(tidyverse)
library(doParallel)
library(reshape2)
# source("J:/Desiree/Sardine/SardineMSE/R/GetSumryOutput.R")
source("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/R/GetSumryOutput.R")

# mseDir <- "J:/Desiree/Sardine/SardineScenarios"
setwd("J:/Desiree/Sardine/SardineScenarios")
#-------------------------------------------
# options()
mseDir <- "J:/Desiree/Sardine/SardineScenarios"
mseDir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios"

# setwd("../SardineScenarios/")
# scenarios <- c("annualGrow2001OM_selfTestSigmaR0dot5CV0dot25_RandRecHCR0",
#                 "annualGrow2001OM_selfTestSigmaR0dot5CV0dot25_RandRecHCR2")
 

# mods <- "constGrow2001OM_constGrow2005EM_"
mods <- "constGrow2001OM_selfTestMidSteep_"
# recscens <- c("ARRecHCR", "RandRecHCR")
recscens <- c("RandRecHCR")
hcrs <- 2

# hcrs <- 1:8
scenarios <- expand_grid(mods, recscens, hcrs) %>% 
  mutate(nn = paste0(mods, recscens, hcrs)) %>% pull(nn)

#Just tested, takes 11 seconds to read in one scenario with settings below
#6 minutes for 2 hcrs, one recscenario and all years
iters <- 1:3
years <- 2020:2038
# termYr <- 2068
em_name <- "constGrowSelfTest_EM_"
om_name <- "constGrowthSteepness0dot6_OM_OM"

#The results directories to read in
resultsdir <- expand_grid(scenarios, iters, em_name, years) %>% 
  mutate(scen = paste0(scenarios, "/", iters, '/', em_name, years)) %>%
  pull(scen)

#Read in the omdirs also
omdir <- expand_grid(scenarios, iters, em_name) %>% 
  mutate(scen = paste0(scenarios, "/", iters, '/', om_name)) %>%
  pull(scen)

# length(resultsdir) * 11 / 100 /60 #18 minutes I think 


####Read in the EM results
start_time <- Sys.time()
ncores <- detectCores() - 2 #Leave some cores open for background stuff
cl <- makeCluster(ncores)
registerDoParallel(cl)

resultsList <- foreach::foreach(ii = 1:length(resultsdir),
                                
                                .packages = c("tidyverse", 'r4ss')) %dopar% {
                                  SS_output(paste(mseDir, resultsdir[ii], sep = "/"), 
                                            covar = FALSE, printstats = FALSE,
                                            verbose = FALSE)
                                }
stopCluster(cl)
run_time <- Sys.time() - start_time; run_time #To see how long it takes
#6 minutes for one scenario; 2 Recreuitmen scenarios, all iterations and all years

#-------------------------------
#OM Results
start_time <- Sys.time()
ncores <- detectCores() - 2 #Leave some cores open for background stuff
cl <- makeCluster(ncores)
registerDoParallel(cl)

omList <- foreach::foreach(ii = 1:length(omdir),
                           
                           .packages = c("tidyverse", 'r4ss')) %dopar% {
                             SS_output(paste(mseDir, omdir[ii], sep = "/"),  
                                       covar = FALSE, printstats = FALSE,
                                       verbose = FALSE)
                           }
stopCluster(cl)
run_time <- Sys.time() - start_time; run_time #To see how long it takes

#-------------------------------------------


#-------------------------------------------
#Summarize things from resultsList


#-----
pull_thing <- function(thing = "timeseries", input = resultsList,
                       scen_names = resultsdir){
  #Pull the thing
  temp <- lapply(input, FUN = function(xx){
    
    return(xx[[thing]])
  })
  
  names(temp) <- scen_names
  #Convert list to a data frame
  temp <- ldply(temp)
  names(temp)[1] <- "scenario"
  temp1 <- ldply(strsplit(temp$scenario, split = "/"))
  names(temp1) <- c("rec_scen", "iter", "mod")
  temp$rec_scen <- temp1$rec_scen 
  temp$iter <- temp1$iter
  temp$mod <- temp1$mod

  parse_scens <- ldply(strsplit(temp$scenario, split = "/"))
  temp$mod <- parse_scens$V1
  temp$iter <- parse_scens$V2
  temp$em <- parse_scens$V3
  if(length(grep("OM", temp$em)) > 0) temp$em <- "OM"
  if(length(grep("OM", temp$em)) == 0){
    temp$est_year <- substr(temp$em, nchar(temp$em)-3, nchar(temp$em))
    temp$est_year <- as.numeric(temp$est_year)
  } 
  
  # temp <- cbind(temp1, temp)
  return(temp)
}

#-----------------------------------------
#Look at recruitment time series in the OMs
omts <- pull_thing("timeseries", input = omList, scen_names = omdir)
omts %>% filter(Yr >= 2002,
                Seas == 1) %>% ggplot(aes(x = Yr, y = Recruit_0, group = iter)) + 
  geom_line()

#-----------------------------------------
#Find models that didn't converge
grads <- pull_thing("maximum_gradient_component")
grads$iter <- as.numeric(grads$iter)

#Look at the differences between gradietn values
grads <- grads %>% group_by(iter) %>% mutate(grad_diff =  c(0, diff(V1))) %>%
  as.data.frame

noconv <- grads %>% filter(V1 > 1e-1) %>% pull(scenario)


#-----------------------------------------
#Compare parameter estimates
ompars <- pull_thing("parameters", input = omList, scen_names = omdir)

ompars <- ompars %>% distinct(Label, Value, rec_scen) 
names(ompars)[2] <- "omValue"

pars <- pull_thing("parameters")  %>% 
  select(scenario, Num, Label, Value, Active_Cnt, Phase, Min, Max, Init, Status, Gradient,
         rec_scen, mod, iter, em, est_year)

#Compare parameter estimates between converged and non-converged
pars$conv <- 1
pars[which(pars$scenario %in% noconv), 'conv'] <- 0
pars$conv <- factor(pars$conv)

pars %>% filter(is.na(Active_Cnt) == FALSE) %>% distinct(Label)

focus_pars <- pars %>% slice(grep("Amin|Amax|_K|R0|CV_|InitAge|InitF", Label))

focus_pars <- focus_pars %>% left_join(ompars, by = c("Label", "rec_scen"))

focus_pars$re <- ((focus_pars$Value - focus_pars$omValue) / focus_pars$omValue) * 100

focus_pars %>% group_by(Label) %>% summarize(p25 = quantile(re, prob=.25), medre = median(re),  p75 = quantile(re, prob = .75))

focus_pars %>% filter(conv == 0) %>% head


focus_pars %>% 
  ggplot(aes(x = est_year, y = Value)) + geom_point(aes(fill = conv),
                                                           pch = 21) +
  facet_wrap(~ Label, scales = 'free_y')

####Correlations between focus_pars
ff <- focus_pars %>% select(scenario, Label, Value) %>% 
  reshape2::dcast(scenario ~ Label, value.var = 'Value')

pairs(ff %>% select(CV_old_Fem_GP_1, 
                    CV_young_Fem_GP_1, 
                    L_at_Amax_Fem_GP_1, 
                    L_at_Amin_Fem_GP_1,
                    "SR_LN(R0)", VonBert_K_Fem_GP_1))

#Kepp only actively estimated
pars <- pars %>% filter(is.na(Active_Cnt) == FALSE)

pars %>% slice(grep("R0|regime_BLK|InitF", Label)) %>% 
  ggplot(aes(x = est_year, y = Value, group = iter)) + 
  facet_wrap(~ Label) + geom_line() + geom_point()

#Look at parameter correlations
corrpars <- pars %>% dcast()
#Non converged
pars %>% filter(Gradient > 1) %>% head
unique(pars$Label)


#Look at parameters for nonconverged models
noconv_pars <- pars %>% filter(scenario %in% noconv)
noconv_pars %>% slice(grep("Amin|Amax|_K|CV_|R0", Label)) %>% 
  ggplot(aes(x = est_year, y = Value)) + geom_point() + facet_wrap(~ Label)
  
  
  filter(Label == "VonBert_K_Fem_GP_1") %>% pull(Value) %>%
  quantile

conv_pars <- pars %>% filter(scenario %in% noconv == FALSE)
conv_pars %>% filter(Label == "VonBert_K_Fem_GP_1") %>% pull(Value) %>%
  quantile

#-----------------------------------------
#Parameter correlations

#-----------------------------------------
#Compare relative error time series
timeseries <- pull_thing("timeseries")
omts <- pull_thing("timeseries", scen_names = omdir, input = omList)


# temp <- smryOutputList$tsSmry
# omts <- temp %>% filter(model_run == "annualGrowth_OM")
omts <- omts %>%  select(Yr, Seas, Bio_smry)  %>% distinct(Yr, Seas, .keep_all = TRUE)
names(omts)[3] <- "OM_Bio_smry"

emts <- timeseries %>%  select(Yr, Seas, Bio_smry, iter, em, est_year)

emts <- emts %>% left_join(omts, by = c("Yr", "Seas"))
emts <- emts %>% filter(Seas == 1) #ONly keep season 1

emts$re_perc <- ((emts$Bio_smry - emts$OM_Bio_smry) / emts$OM_Bio_smry) * 100

ggplot(emts, aes(x = Yr, y = re_perc)) + geom_point()

#Plot the median and 90th percentiles of relative error
relerr <- emts %>% group_by(Yr) %>% summarize(median_re = median(re_perc), p95 = quantile(re_perc, .95),
                                      p5 = quantile(re_perc, .05)) 
 
relerr %>% reshape2::melt(id.var = "Yr") %>% filter(variable != "p95") %>% 
  ggplot(aes(x = Yr, y = value, linetype = variable)) +
  geom_line() + geom_point()




#-----------------------------------------
#Check one of the results
temp <- resultsList[[50]]
names(resultsList) <- resultsdir



omts %>% filter(Yr == 2020, Seas == 1) #Check that OM value is the same


#Match the EM with the OM values

unique(temp$iteration)


#-----------------------------------------
str


performanceList <- CalcPerformance(smryOutputList)

metricsTbl <- performanceList$perfomanceMetrics

# parse out HCR and recruitment scenario
metricsTbl <- metricsTbl %>% mutate(HCR = sub(pattern = ".*Rec","", scenario),
                                    recScen = sub(pattern = "HCR.*","", scenario)) %>%
  mutate(recScen = sub(pattern = ".*selfTest_","", recScen))

hcrPal <- brewer.pal(10, "Set3")[-2]
