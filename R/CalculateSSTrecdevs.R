library(tidyverse)

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
sstDat <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/SST_CC_ann_PFMC2013AppxG.csv")
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
meanRecDevSST <- MakeRecruitDevs(envtInx = mean(sstDat$SST_CC_ann),
                                 envtCoeff = 1.280965e+00, devSD = 0.00001) # no error in baseline ref temp
# deviation is the difference between yearly temp-dependent and mean temp-dependent adjustment?
sstDat <- sstDat %>% mutate(recDevSSTDiff = recDevSST - meanRecDevSST)
range(sstDat$recDevSSTDiff)

sstDat %>% ggplot(aes(x = Year, y = recDevSSTDiff)) + geom_line() +
  geom_hline(yintercept = 0, color = "darkgrey")

# Read in projected CalCOFI SST data
projSST <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/calcofi_sst_projected.csv")

# Calculate recdevs for each projection
projSST$recDevSST_GFDL <- MakeRecruitDevs(envtInx = projSST$gfdl_sst_all,
                                    envtCoeff = 1.280965e+00, devSD = 0.45) # R^2 of annual SST model was 0.55 (PFMC 2013, Appendix E, pg 49) 

projSST$recDevSST_HAD <- MakeRecruitDevs(envtInx = projSST$had_sst_all,
                                    envtCoeff = 1.280965e+00, devSD = 0.45) 

projSST$recDevSST_IPSL <- MakeRecruitDevs(envtInx = projSST$ipsl_sst_all,
                                    envtCoeff = 1.280965e+00, devSD = 0.45) 

# deviation is the difference between yearly temp-dependent and mean temp-dependent adjustment
projSST <- projSST %>% mutate(recDevSST_GFDL = recDevSST_GFDL - meanRecDevSST,
                              recDevSST_HAD = recDevSST_HAD - meanRecDevSST,
                              recDevSST_IPSL = recDevSST_IPSL - meanRecDevSST)

# modify rec devs so historical period (pre-2005) is centered on 0
centerGFDL <- projSST %>% filter(year < 2005) %>% select(recDevSST_GFDL)
centerGFDL <- scale(centerGFDL, center = TRUE, scale = FALSE)
centerHAD <- projSST %>% filter(year < 2005) %>% select(recDevSST_HAD)
centerHAD <- scale(centerHAD, center = TRUE, scale = FALSE)
centerIPSL <- projSST %>% filter(year < 2005) %>% select(recDevSST_IPSL)
centerIPSL <- scale(centerIPSL, center = TRUE, scale = FALSE)

projSST <- projSST %>% mutate(recDevSST_GFDL = recDevSST_GFDL - attr(centerGFDL,"scaled:center"),
                              recDevSST_HAD = recDevSST_HAD - attr(centerHAD,"scaled:center"),
                              recDevSST_IPSL = recDevSST_IPSL - attr(centerIPSL,"scaled:center"))
# bias correction
sdSST <- projSST %>% filter(year >2019) %>% 
          summarize(devSD_GFDL = sd(recDevSST_GFDL),
                    devSD_HAD = sd(recDevSST_HAD),
                    devSD_IPSL = sd(recDevSST_IPSL))

projSST <- projSST %>% mutate(recDevSST_GFDL = recDevSST_GFDL * (1.25/sdSST$devSD_GFDL),
                              recDevSST_HAD = recDevSST_HAD * (1.25/sdSST$devSD_HAD),
                              recDevSST_IPSL = recDevSST_IPSL * (1.25/sdSST$devSD_IPSL))

#write.csv(projSST, "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevSST2070.csv")
