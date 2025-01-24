# Explore data for sardine recruitment indices

library(tidyverse)
library(r4ss)

# From Peter Kuriyama, 9/28/2021:
# Here's the weight-at-age file used in the sardine assessment. If you load r4ss 
# you can read the values in with SS_readwtatage(file="wtatage.ss"). Once loaded, 
# the values for Fleet 4 are from the survey and season 1 is summer (July to 
# December) and season 2 is spring (January to June of following calendar year). 

wtAgeDat <- SS_readwtatage(file="C:/Users/r.wildermuth/Documents/FutureSeas/RecruitmentIndex/wtatage.ss")
test1 <- wtAgeDat %>% filter(Fleet == 4, Seas == 2)
matplot(test1$Yr, test1[, 9:ncol(test1)], type = "l", ylim = c(0, 0.5))

# compare to estimated recruitment from the research model
researchResults <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment")
researchResults$recruit
lines(researchResults$recruit$Yr, (scale(log(researchResults$recruit$exp_recr))/5)+0.25, col = "darkblue")

lines(test1$Yr, (scale(rowMeans(test1[, 9:ncol(test1)]))/5)+0.25, col = "red")

# From Stefan Koenegstein, 1/27/2022:
# Here’s the MICE output we talked about earlier today. Includes the calibration 
# period (RA, reanalysis) for comparison although you may not need it and it should 
# be a good fit with the stock assessment data. 
# Recruits are total yearly new Age1 individuals when promoted from Age0. Stock 
# values are averaged over the year and not shifted yet (i.e. you'd have to lag 
# the adult numbers by one year to compare with recruits from the current year). 
# Below is an overview graph just using total adult number… to me it looks like 
# it should work well to fit a B-H curve!
# Mean: the ensemble mean over all model configurations and early life stage thermal windows.
# Mean-ELSdriven: Same over a selection of 5 of 9 model configurations which are mainly driven by ELS temperature and food, not adult consumption (i.e. only those configurations which still show a good historical fit when keeping adult food constant over time). Maybe worth a test if there’s a difference to the mean of all.
# Max: Upper confidence boundary, highest ensemble configuration with high optimal T for ELS
# Min: Lower confidence boundary, sardine population collapse after passing an early optimal T for ELS. 
# Those latter two just maybe for tests (note absolute values relative to the calibration period in the figure).

srMICE <- read_csv(file = "C:/Users/r.wildermuth/Documents/FutureSeas/MICE stock-recruits.csv")
srMICE <- srMICE %>% mutate(mean_k = mean/1000, # get numbers in 1000s to match SS output
                            min_k = min/1000,
                            max_k = max/1000,
                            meanELSdriven_k = meanELSdriven/1000)
test2 <- srMICE %>% filter(ModelParam %in% c("adult.stock.sum", "recruits")) %>%
            pivot_wider(id_cols = c(year, GCM), names_from = ModelParam, values_from = meanELSdriven)
test2 %>% ggplot(aes(x = adult.stock.sum, y = recruits)) +
  geom_point() +
  facet_wrap(vars(GCM))

# Beverton-Holt Spawner-Recruit curve w/ bias adjustment from SS3.30 manual pg 85
CalcRecruitsBH <- function(SBy, # spawner biomass in year y
                           lnR0, # natural log of unfished equilibrium recruitment
                           h, # steepness
                           recSD = 1){ # standard deviation of recruitment devs
  R0 <- exp(lnR0)-2.25458 # !!RW: not sure how to implement regime adjustment
  SB0 <- 1.32931e+006 # pulled SSB_unfished from Report.sso file  
  Ry <- (4 * h * R0 * SBy)/(SB0 * (1-h) + SBy * (5*h - 1))
  Ry <- Ry * exp(-0.5 * 0.9365 * recSD) # bias adjustment
  
  return(Ry)
}

# Convert MICE abundances to biomass using average weight at age

# get mean yearly weight-at-age
avgWtAge <- wtAgeDat %>% pivot_longer(cols = c('0', '1', '2', '3', '4', '5', 
                                               '6', '7', '8', '9', '10'), 
                                      names_to = "Age", values_to = "indivWt") %>%
              select(Yr, Seas, Fleet, Age, indivWt) %>%
              filter(Fleet == 4) # want

seas2Age0 <- avgWtAge %>% filter(Seas == 2, Age == 0) %>%
              summarize(indivWtAvg = mean(indivWt))
age8plus <- avgWtAge %>% filter(Age %in% c('8', '9', '10')) %>%
              summarize(indivWtAvg = mean(indivWt))

avgWtAge <- avgWtAge %>% group_by(Age) %>% summarize(indivWtAvg = mean(indivWt)) 

avgWtAge[avgWtAge$Age == '0', "indivWtAvg"] <- seas2Age0
avgWtAge <- rbind(avgWtAge, data.frame(Age = "age8plus", indivWtAvg = age8plus))

# maturity at age data provided by Peter Kuriyama 2/9/2022
matAtAge <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/maturityAtAge_02092022.csv")
# change proportion for age 0 to get only spawning biomass of ages 1+
matAtAge <- matAtAge %>% mutate(Age = as.character(Age),
                                PropMature = case_when(Age == "0" ~ 1,
                                                       TRUE ~ PropMature))

srMICE <- srMICE %>% filter(ModelParam != "adult.stock.sum") %>%
            mutate(Age = str_extract(ModelParam,"\\d+")) %>%
            mutate(Age = case_when(is.na(Age) ~ "0",
                                   TRUE ~ Age)) %>%
            left_join(y = avgWtAge, by = "Age") %>%
            left_join(y = matAtAge, by = "Age") %>%
            mutate(meanBiom_kg = mean * indivWtAvg * PropMature, # mean mature weight by Age, but not for age 0
                   meanELSBiom_kg = meanELSdriven * indivWtAvg * PropMature)

recs <- srMICE %>% filter(ModelParam == "recruits")

ssb <- srMICE %>% filter(ModelParam != "recruits") %>%
          group_by(year, GCM) %>%
          summarize(meanSSB = sum(meanBiom_kg) * 0.001, # convert to metric tons
                    meanELSSSB = sum(meanELSBiom_kg) * 0.001) %>%
          mutate(yearPlus1 = year + 1)

ssbrecsMICE <- ssb %>% left_join(y = recs, by = c("GCM", "yearPlus1" = "year"), keep = TRUE)

# Calculate the S-R curve as specified in the sardine SS research model control file 

x <- seq(0, 2e6, by = 1000000)
y <- sapply(x, CalcRecruitsBH, lnR0 = 15.3516, # natural log of unfished equilibrium recruitment
            h = .5, # steepness
            recSD = 1) # sigma R

ssbrecsMICE %>% ggplot(aes(x = meanSSB, y = mean_k)) +
  geom_point() +
  facet_wrap(vars(GCM.x), scales = "free")+
  geom_line(data = as.data.frame(cbind(x,y)), 
            aes(x=x,y=y))
ssbrecsMICE %>% ggplot(aes(x = meanELSSSB, y = meanELSdriven_k)) +
  geom_point() +
  facet_wrap(vars(GCM.x), scales = "free") +
  geom_line(data = as.data.frame(cbind(x,y)), 
            aes(x=x,y=y))

ssbrecsMICE %>% filter(GCM.x == "RA") %>%
    summarize(minNum = min(mean, na.rm = TRUE),
              minELSNum = min(meanELSdriven, na.rm = TRUE),
              maxNum = max(mean, na.rm = TRUE),
              maxELSNum = max(meanELSdriven, na.rm = TRUE))

# compare to estimated recruitment from the research model
st1981 <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment")

range(st1981$recruit$pred_recr)

x1981 <- seq(0, max(st1981$recruit$SpawnBio)+100, by = 10000)
y1981 <- sapply(x1981, CalcRecruitsBH, lnR0 = 15.3516, # natural log of unfished equilibrium recruitment
            h = .5, # steepness
            recSD = 1.) # sigma R

st1981$recruit %>% filter(era == "Main") %>%
  ggplot(aes(x=SpawnBio, y=log(pred_recr))) + geom_point() +
  geom_line(data = as.data.frame(cbind(x1981,y1981)), 
            aes(x=x1981,y=log(y1981))) 

ssbrecsMICE %>% filter(GCM.x == "RA") %>% ggplot(aes(x = year.x, y = log(meanSSB))) +
  geom_line() +
  geom_line(data = st1981$recruit, aes(x = Yr, y = log(SpawnBio)))

# compare to estimated recruitment from the 2001 model
st2001 <- SS_output(dir = "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/scenarioModels/start2001/constGrowthMidSteepNewSelex_OM")

range(st2001$recruit$pred_recr)

x2001 <- seq(0, max(st2001$recruit$SpawnBio)+100, by = 10000)
y2001 <- sapply(x2001, CalcRecruitsBH, lnR0 = 15.3516, # natural log of unfished equilibrium recruitment
            h = .3, # steepness
            recSD = 1.25) # sigma R

st2001$recruit %>% filter(era == "Main") %>%
    ggplot(aes(x=SpawnBio, y=pred_recr)) + geom_point() +
  geom_line(data = as.data.frame(cbind(x2001,y2001)), 
            aes(x=x2001,y=y2001)) 

ssbrecsMICE %>% filter(GCM.x == "RA") %>% ggplot(aes(x = year.x, y = log(meanSSB))) +
  geom_line() +
  geom_line(data = st2001$recruit, aes(x = Yr, y = log(SpawnBio)))

# compare all recruitments and S-R curves
compareSR <- ssbrecsMICE %>% filter(GCM.x == "RA") %>% 
                select(year.y, meanSSB, meanELSSSB, mean_k, meanELSdriven_k) %>%
                rename("year" = "year.y",
                       "MICE_SSB" = "meanSSB",
                       "MICE.ELS_SSB" = "meanELSSSB",
                       "MICE_Recruit" = "mean_k",
                       "MICE.ELS_Recruit" = "meanELSdriven_k") %>%
                pivot_longer(cols = !year, names_to = c("Model", "Measure"),
                             names_sep = "_", values_to = c("value")) %>%
                pivot_wider(names_from = "Measure", values_from = "value")

sr2001 <- st2001$recruit %>% filter(era == "Main") %>% select(Yr, SpawnBio, pred_recr) %>%
            mutate(Model = "st2001") %>%
            rename("year" = "Yr",
                   "SSB" = "SpawnBio",
                   "Recruit" = "pred_recr")

sr1981 <- st1981$recruit %>% filter(era == "Main") %>% select(Yr, SpawnBio, pred_recr) %>%
            mutate(Model = "st1981") %>%
            rename("year" = "Yr",
                   "SSB" = "SpawnBio",
                   "Recruit" = "pred_recr")

compareSR <- rbind(compareSR, sr2001, sr1981)

srCurves <- data.frame(ssb = c(x1981, x2001),
                       rec = c(y1981, y2001),
                       srModel = c(rep("st1981", length(x1981)), 
                                 rep("st2001", length(x2001))))

compareSR %>% ggplot(aes(x=SSB, y=log(Recruit), color = Model)) + geom_point() +
  facet_wrap(vars(Model)) +
  geom_line(data = srCurves, 
            aes(x=ssb,y=log(rec), color = srModel))

# Compare numbers at age and recruits among models ------------------------

# st1981NatAge <- st1981$natage_annual_2_with_fishery %>% 
st1981NatAge <- st1981$natage %>% filter(Era == "TIME", Seas == 2, `Beg/Mid` == "B") %>%
                  pivot_longer(cols = c('0', '1', '2', '3', '4', '5', 
                                        '6', '7', '8', '9', '10'),
                               names_to = "Age", values_to = "natage1981")
# st2001NatAge <-st2001$natage_annual_2_with_fishery %>% 
st2001NatAge <-st2001$natage %>% filter(Era == "TIME", Seas == 2, `Beg/Mid` == "B") %>%
                  pivot_longer(cols = c('0', '1', '2', '3', '4', '5', 
                                        '6', '7', '8', '9', '10'),
                               names_to = "Age", values_to = "natage2001")

compareNatAge <- srMICE %>% filter(GCM == "RA") %>%
                    left_join(y = st1981NatAge, by = c("year" = "Yr", "Age")) %>%
                    left_join(y = st2001NatAge, by = c("year" = "Yr", "Age")) %>%
                    mutate(errMICEto1981 = (mean_k - natage1981)/natage1981,
                           err2001to1981 = (natage2001 - natage1981)/natage1981)

compareNatAge %>% select(year, Age, natage1981, natage2001, mean_k, meanELSdriven_k) %>%
  pivot_longer(cols = c(natage1981, natage2001, mean_k, meanELSdriven_k), 
               names_to = "model",
               values_to = "NatAge")%>% #filter(model %in% c("natage1981", "natage2001")) %>%
  ggplot(aes(x = year, y = NatAge, color = model)) +
  geom_line() + 
  ggplot2::facet_wrap(~ Age, scales = "free")

compareNatAge %>% select(year, Age, errMICEto1981, err2001to1981) %>%
                    pivot_longer(cols = c(errMICEto1981, err2001to1981), 
                                 names_to = "comp1981",
                                 values_to = "relativeError")%>%
# compareNatAge %>% filter(comp1981 != "err2001to1981", !is.na(relativeError)) %>%
  ggplot(mapping = aes(x = year, y = relativeError, color = comp1981)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "black") +
  ggplot2::facet_wrap(~ Age, scales = "free") +
  ggplot2::theme_classic() +
  ylab("Relative Error (N at Age)")


# Try and fit new S-R model to MICE output --------------------------------

nlsFitBH <- nls(log(mean_k) ~ log(CalcRecruitsBH(SBy = meanSSB, lnR0, h, recSD = 1.25)),
                data = ssbrecsMICE[ssbrecsMICE$GCM.x == "RA", ], 
                start = list(lnR0 = 16.83091, h = 0.5))
summary(nlsFitBH)

# try MLE estimation

normNegLogLik <- function(obsRy, SBy, lnR0, h, recSD){
  
  # Beverton-Holt Spawner-Recruit curve w/ bias adjustment from SS3.30 manual pg 85
  # CalcRecruitsBH <- function(SBy, # spawner biomass in year y
  #                            lnR0, # natural log of unfished equilibrium recruitment
  #                            h, # steepness
  #                            recSD = 1){ # standard deviation of recruitment devs
    R0 <- exp(lnR0)-2.25458 # !!RW: not sure how to implement regime adjustment
    SB0 <- 1.32931e+006 # pulled SSB_unfished from Report.sso file  
    Ry <- (4 * h * R0 * SBy)/(SB0 * (1-h) + SBy * (5*h - 1))
    RyHat <- Ry * exp(-0.5 * 0.9365 * recSD) # bias adjustment
    
  #   return(Ry)
  # }
  
  # RyHat <- CalcRecruitsBH(SBy = SBy, lnR0 = lnR0, h = h, recSD = recSD)
  -sum(dnorm(x = obsRy/RyHat, mean = 0, sd = recSD, log = TRUE))
}

test3 <- ssbrecsMICE %>% filter(GCM.x == "RA") %>% 
            select("year.x", "mean_k", "meanSSB") %>% filter(complete.cases(.))
normNegLogLik(obsRy = test3$mean_k,
              SBy = test3$meanSSB,
              lnR0 = 16, h = 0.3, recSD = 1)

mleFitBH <- stats4::mle(minuslogl = normNegLogLik(obsRy = test3$mean_k,
                                                  SBy = test3$meanSSB,
                                                  lnR0, h, recSD),
                        start = list(lnR0 = 15.3516, h = 0.5), 
                        fixed = list(recSD = 1))

# From Desiree Tommasi's S_R_fit_example.R

lnr0 <- 15.3516 #from control file
h <- 0.5 #from control file
sb0 <- st1981$derived_quants["SSB_Virgin", "Value"] #virgin ssb from derived quantities

#we use the common BH formulation, so we need to calculate a and b from the 
# we use as the SS quantities
b <- sb0*(1-h)/(5*h-1)
a <- (4*h*exp(lnr0)/(5*h-1))

#Beverton Holt function
BH_lin <- function(pars,sb) {
  a <- pars[1]
  b <- pars[2]
  recl <- log(a*sb/(b+sb))
  return(recl)
}

#negative log likelihood function. We assume the sigma is the same as the st2001
# stock assessment and study OM
nll <- function(pars, data) {
  # Values predicted by the model
  Rpred <- BH_lin(pars, data$ssb)
  Robs <- log(data$rec)
  # Negative log-likelihood 
  -sum(dnorm(x = Robs, mean = Rpred, sd = 1.25, log = TRUE))
}

#use optim to estimate the parameters - choosing initial values that are pretty far from actual
datMICE <- ssbrecsMICE %>% filter(GCM.x == "RA") %>% select(meanSSB, mean) %>%
              rename("ssb" = "meanSSB", 
                     "rec" = "mean") %>%
              filter(complete.cases(.))
fitMICE <- optim(par = c(1000,10), fn = nll, method = "L-BFGS-B",
                 lower = rep(2, 1), data = datMICE)
# try different starting vals to make sure getting minimum nll
fitMICE1 <- optim(par = c(1e11,10), fn = nll, method = "L-BFGS-B",
                 lower = rep(2, 1), data = datMICE)
fitMICE2 <- optim(par = c(1000,1e10), fn = nll, method = "L-BFGS-B",
                 lower = rep(2, 1), data = datMICE)
fitMICE3 <- optim(par = c(1e11,1e10), fn = nll, method = "L-BFGS-B",
                 lower = rep(2, 1), data = datMICE)
fitMICE4 <- optim(par = c(1e5,1e5), fn = nll, method = "L-BFGS-B",
                  lower = rep(2, 1), data = datMICE)
fitMICE5 <- optim(par = c(1e2,1e10), fn = nll, method = "L-BFGS-B",
                  lower = rep(2, 1), data = datMICE)
fitMICE6 <- optim(par = c(1e11,1e5), fn = nll, method = "L-BFGS-B",
                  lower = rep(2, 1), data = datMICE)
fitMICE7 <- optim(par = c(1e5,1e10), fn = nll, method = "L-BFGS-B",
                  lower = rep(2, 1), data = datMICE)

diffFits <- rbind(fitMICE$par, fitMICE1$par, fitMICE2$par, fitMICE3$par,
                  fitMICE4$par, fitMICE5$par, fitMICE6$par, fitMICE7$par)
colnames(diffFits) <- c("a", "b")
diffFits <- as.data.frame(diffFits)
  
diffFits %>% mutate(nll = c(fitMICE$value, fitMICE1$value, 
                            fitMICE2$value, fitMICE3$value,
                            fitMICE4$value, fitMICE5$value,
                            fitMICE6$value, fitMICE7$value),
                    convg = c(fitMICE$convergence, fitMICE1$convergence, 
                              fitMICE2$convergence, fitMICE3$convergence,
                              fitMICE4$convergence, fitMICE5$convergence,
                              fitMICE6$convergence, fitMICE7$convergence)) %>%
  arrange(convg, nll)

test4 <- optim(par = c(20000000000, 1000000), fn = nll, method = "L-BFGS-B",
                  lower = rep(2, 1), data = datMICE)

#check fit to the simulated rec data
plot(datMICE$ssb,log(datMICE$rec),ylim=c(10,24))
curve(BH_lin(fitMICE2$par, x), 0, 5e+7,n=100000, add = TRUE,col="green")
curve(BH_lin(c(a,b), x), 0, 5e+7,n=100000, add = TRUE,col="blue")#note this is using the S_R param from the research model

#calculate the rec devs
histMICEdevs <- log(datMICE$rec)-BH_lin(fitMICE2$par, datMICE$ssb)
ssbrecsMICE$meanDevs <- log(ssbrecsMICE$mean)-BH_lin(fitMICE2$par, sb = ssbrecsMICE$meanSSB)
ssbrecsMICE$meanELSDevs <- log(ssbrecsMICE$meanELSdriven)-BH_lin(fitMICE2$par, sb = ssbrecsMICE$meanELSSSB)
ggplot(data=ssbrecsMICE,aes(x = year.x, y = meanDevs,group=as.factor(GCM.x),color=as.factor(GCM.x))) +
  geom_line()

ssbrecsMICE <- ssbrecsMICE %>% select(year.y, GCM.y, meanSSB, mean, meanDevs, 
                                      meanELSSSB, meanELSdriven, meanELSDevs) %>%
                  rename("Year" = "year.y",
                         "GCM" = "GCM.y", 
                         "ensembleSSB" = "meanSSB",
                         "ensembleRec" = "mean",
                         "ensembleRecDevs" = "meanDevs",
                         "elsSSB" = "meanELSSSB",
                         "elsRecs" = "meanELSdriven",
                         "elsRecDevs" = "meanELSDevs")%>%
                  filter(complete.cases(.))


sdMICE <- ssbrecsMICE %>% filter(GCM == "GFDL") %>% 
            summarize(ensembleDevSD = sd(ensembleRecDevs),
                      elsDevSD = sd(elsRecDevs))

# add 0 deviation for missing 2019 and 2020 and do bias correction 
ssbrecsMICE <- ssbrecsMICE %>% add_case(Year = rep(2019:2020, each = 3),
                                        GCM = rep(c("GFDL", "HAD", "IPSL"), times = 2),
                                        ensembleRecDevs = rep(0,times = 6),
                                        elsRecDevs = rep(0, times = 6)) %>%
                  arrange(Year)

# Create model average
ensMean <- ssbrecsMICE %>% filter(Year > 2018) %>%
              group_by(Year) %>%
              summarize(GCM = "gcmMEAN",
                        ensembleSSB = mean(ensembleSSB),
                        ensembleRec = mean(ensembleRec),
                        ensembleRecDevs = mean(ensembleRecDevs),
                        elsSSB = mean(elsSSB),
                        elsRecs = mean(elsRecs),
                        elsRecDevs = mean(elsRecDevs))

ssbrecsMICE <- rbind(ssbrecsMICE, ensMean)
  
ggplot(data=ssbrecsMICE,
       aes(x = Year, y = ensembleRecDevs, group=as.factor(GCM), color=as.factor(GCM))) +
  geom_line()

# write.csv(ssbrecsMICE, "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevMICE2100.csv")

ssbrecsMICE %>% filter(GCM != "gcmMEAN") %>%
  ggplot(aes(x = Year, y = ensembleRecDevs, group=as.factor(GCM), color=as.factor(GCM))) +
  geom_vline(xintercept = 2019.5, color = "gray", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(y = "Recruitment Deviations", color = "ESM") 
  
datMICE %>% mutate(exLogRec = BH_lin(fitMICE2$par, ssb)) %>%
  ggplot() +
  geom_point(aes(x = ssb, y = log(rec))) +
  geom_line(aes(x = ssb, y = exLogRec), linewidth = 1) +
  theme_classic() +
  labs(y = "Recruits (log)", x = "SSB")

ssbrecsMICE %>% filter(GCM %in% c("RA", "gcmMEAN")) %>%
  mutate(exLogRec = BH_lin(fitMICE2$par, ensembleSSB)) %>%
  ggplot() +
  geom_point(aes(x = ensembleSSB, y = log(ensembleRec), color = GCM)) +
  geom_line(aes(x = ensembleSSB, y = exLogRec), linewidth = 1) +
  theme_classic() +
  scale_color_manual(values = c("steelblue", "black"), 
                    labels = c("Projected", "Historical"), name = "") +
  labs(y = "Recruits (log)", x = "SSB")
