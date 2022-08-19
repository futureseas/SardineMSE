# Code to calculate recruitment deviations based on the SST relationship defined
# in PFMC 2013
# Robert Wildermuth

library(tidyverse)
library(ggplot2)
library(r4ss)

source("R/MakeRecruitDevs.R")

# First, explore function by Larry Jacobson in PFMC 2013:
############################################################
# VERSION 2
#A parametric approximation to the best sardine GAM model
# for recruitment as a function of SSB and SST based on the
# linear model shown above. Take care in applying the
# function to SST levels outside of 14.9-16.8 degrees C
# and spawning biomass levels above 1500 thousand t.
# Larry Jacobson, Feb. 10, 2013
ApproxSardineSR<-function(SSB,SST,FlatTop=FALSE){
  #
  #parameters for polynomial regression (first step)
  a <- -1.338750e+01
  B1 <- 4.999496e-03
  B2<- -2.600599e-06
  T1<- 1.280965e+00
  ansr<-a + B1*SSB + B2*SSB^2 + T1*SST
  if (FlatTop) {
    #parameters for flat top adjustment
    # start by finding the biomass where R starts to decline (use derivative)
    ssbGoesFlat<- -B1/(2*B2)
    maxlnR<- a + B1*ssbGoesFlat + B2*ssbGoesFlat^2 +
      T1*SST
    ansr<-ifelse(SSB>ssbGoesFlat,maxlnR,ansr)
  }
  return(exp(ansr))
}

# load SST data in the PFMC doc
sstDat <- read_csv("../SardineMSE/dat/SST_CC_ann_PFMC2013AppxG.csv")
summary(sstDat)

# calculate and plot S-R curve for different levels of SST
srSST <- data.frame(ssb_kt = seq(0, 10000, by = 100),
                    recSST14.9 = NA,
                    recSST15.83 = NA,
                    recSST16.8 = NA)

srSST <- srSST %>% mutate(recSST14.9 = ApproxSardineSR(SSB = ssb_kt,SST = 14.9,FlatTop=TRUE),
                          recSST15.83 = ApproxSardineSR(SSB = ssb_kt,SST = 15.83,FlatTop=TRUE),
                          recSST16.8 = ApproxSardineSR(SSB = ssb_kt,SST = 16.8,FlatTop=TRUE))

srSST %>% pivot_longer(cols = c(recSST14.9, recSST15.83, recSST16.8),
                       names_to = "recSST", values_to = "recruits") %>%
  ggplot(aes(x = ssb_kt, y = recruits, color = recSST)) +
  geom_line()
# Create recruitment deviation time series for SST projection ------------

# apply temperature effect coefficient from Appendix H of PFMC 2013, DRAFT Report 
#   of the Pacific Sardine Harvest Parameters Workshop
sstDat$recDevSST <- MakeRecruitDevs(envtInx = sstDat$SST_CC_ann,
                                    envtCoeff = 1.280965e+00, devSD = 0.45) 
histMean <- mean(sstDat$SST_CC_ann)
# meanRecDevSST <- MakeRecruitDevs(envtInx = histMean,
#                                  envtCoeff = 1.280965e+00, devSD = 0.00001) # no error in baseline ref temp
# calculate straight up mean temp effect
meanRecDevSST <- histMean * 1.280965e+00
# deviation is the difference between yearly temp-dependent and mean temp-dependent adjustment?
sstDat <- sstDat %>% mutate(recDevSSTDiff = recDevSST - meanRecDevSST)
range(sstDat$recDevSSTDiff)

sstDat %>% ggplot(aes(x = Year, y = recDevSSTDiff)) + geom_line() +
  geom_hline(yintercept = 0, color = "darkgrey")

# Read in projected CalCOFI SST data
projSST <- read_csv("../SardineMSE/dat/calcofi_sst_projected.csv")

#add observed SST used for the PFMC analysis
projSST <- projSST %>% full_join(y = sstDat %>% select(Year, SST_CC_ann), 
                                 by = c("year" = "Year"))

# change to long format for plotting
projSST %>%
  pivot_longer(!c(year, gfdl_sst_station, had_sst_station, ipsl_sst_station), 
               names_to = "model", values_to = "sst") %>%
  ggplot(aes(x=year, y=sst, group=model, colour=model)) +
         geom_line() +
         geom_point()

#bias correct the projections using the SST observations from 1984-2008
sstMeans <- projSST %>% filter(year %in% (1984:2008)) %>%
              summarize(meanGFDL = mean(gfdl_sst_all),
                        meanHAD = mean(had_sst_all),
                        meanIPSL = mean(ipsl_sst_all),
                        meanObs = mean(SST_CC_ann, na.rm = TRUE))

projSST <- projSST %>% mutate(sstGFDLbc = gfdl_sst_all - sstMeans$meanGFDL + sstMeans$meanObs,
                              sstHADbc = had_sst_all - sstMeans$meanHAD + sstMeans$meanObs,
                              sstIPSLbc = ipsl_sst_all - sstMeans$meanIPSL + sstMeans$meanObs,)

#create ensemble mean
projSST$emean <- rowMeans(projSST[,c("sstGFDLbc", "sstHADbc", "sstIPSLbc")])

projSST %>%
  pivot_longer(c(sstGFDLbc, sstHADbc, sstIPSLbc, emean, SST_CC_ann),
               names_to = "model", values_to = "sst") %>%
  ggplot(aes(x=year, y=sst, group=model, colour=model)) +
  geom_line() 

# Bias correct projections and compute ensemble mean
projSST <- projSST %>% mutate(recDevSST_GFDL = (sstGFDLbc * 1.280965e+00) - meanRecDevSST,
                              recDevSST_HAD = (sstHADbc * 1.280965e+00) - meanRecDevSST,
                              recDevSST_IPSL = (sstIPSLbc * 1.280965e+00) - meanRecDevSST,
                              recDevSST_EMEAN = (emean * 1.280965e+00) - meanRecDevSST)

# bias correction
sdSST <- projSST %>% filter(year >2019) %>% 
          summarize(devSD_GFDL = sd(recDevSST_GFDL),
                    devSD_HAD = sd(recDevSST_HAD),
                    devSD_IPSL = sd(recDevSST_IPSL),
                    devSD_EMEAN = sd(recDevSST_EMEAN))

projSST <- projSST %>% mutate(recDevSST_GFDLbc = recDevSST_GFDL * (1.25/sdSST$devSD_GFDL),
                              recDevSST_HADbc = recDevSST_HAD * (1.25/sdSST$devSD_HAD),
                              recDevSST_IPSLbc = recDevSST_IPSL * (1.25/sdSST$devSD_IPSL),
                              recDevSST_EMEANbc = recDevSST_EMEAN * (1.25/sdSST$devSD_EMEAN))

# plot of various recruitment deviation estimates
plot(projSST$year, projSST$recDevSST_EMEAN, type = "l", ylim = c(-4,6), 
     main = "Rec devs from SST")
lines(projSST$year, projSST$recDevSST_GFDLbc, col = 4)
lines(projSST$year, projSST$recDevSST_HADbc, col = 2)
lines(projSST$year, projSST$recDevSST_IPSLbc, col = 3)
abline(h = 0, col = "grey")
abline(v = 2019.5, lty = 2, col = "grey")



# write.csv(projSST, "../SardineMSE/dat/recdevSST2070.csv")
