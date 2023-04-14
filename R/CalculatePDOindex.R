# Code to calculate the 60-yr PDO cycle and project sardine recruitment deviations
# based on cyclic or ROMS projected PDO.
# Robert Wildermuth

library(tidyverse)
library(r4ss)
source("R/MakeRecruitDevs.R")

# PDO data downloaded from CalCurrent IEA website: https://www.integratedecosystemassessment.noaa.gov/index.php/regions/california-current/california-current-iea-indicators
moPDO <- read.csv("C:/Users/r.wildermuth/Documents/FutureSeas/RecruitmentIndex/monthlyPDO.csv")

# remove last year (2021)
moPDO <- moPDO %>% filter(Year < 2021)
moPDO$yrPDO <- NA #rowMeans(moPDO[, 2:13])

moPDO$sumPDO <- rowMeans(moPDO[, c("augPDO", "sepPDO", "octPDO")])
moPDO$sprPDO <- rowMeans(moPDO[, c("marPDO", "aprPDO", "mayPDO", 
                                   "junPDO", "julPDO")])

for(i in 2:nrow(moPDO)){
  moPDO$yrPDO[i] <- moPDO$sumPDO[i-1] + moPDO$sprPDO[i]
}

moPDO %>% select(Year, sumPDO, sprPDO, yrPDO)

myFit <- lm(yrPDO ~ sin(2*pi*Year/60), data = moPDO)
myFit
summary(myFit)

moPDO$expPDO <- c(NA, predict(myFit))

moPDO %>% select(Year, sumPDO, sprPDO, yrPDO, expPDO)

newDat <- data.frame(Year = 2021:2120)
newDat$expPDO <- predict(myFit, newdata = newDat)

ggplot(data = moPDO, aes(x=Year, y=expPDO)) +
  geom_line() +
  geom_point(aes(y = yrPDO)) +
  geom_line(data = newDat, aes(col = "Projection"))


# Create recruitment deviation time series for PDO projection ------------

# Need to start with year 2019
newDat <- rbind(moPDO[moPDO$Year %in% 2019:2020, c("Year", "expPDO")],
                newDat)

# project rec devs using fit in Appendix A of Zwolinski & Demer 2019
# newDat$recDevPDO <- MakeRecruitDevs(envtInx = newDat$expPDO,
#                                     envtCoeff = 0.7815, 
#                                     devSD = 0.56) # pseudo-R^2 of PDO fit was 0.44 in Zwolinski & Demer 2019
newDat <- newDat %>% mutate(recDevPDO = expPDO * 0.7815)
                              
# make rec_devs with SS sigmaR as 'devSD'
newDat$recDevSS_cyclPDO <- MakeRecruitDevs(envtInx = newDat$expPDO,
                                           envtCoeff = 0.7815, 
                                           devSD = 1.25) 
# bias correction
sdPDO <- newDat %>% filter(Year < 2070) %>%
            summarize(devSD = sd(recDevPDO),
                      devSD_SS = sd(recDevSS_cyclPDO))

newDat <- newDat %>% mutate(recDevPDObc = recDevPDO * (1.25/sdPDO$devSD))
#                             recDevSS_cyclPDO = recDevSS_cyclPDO * (1.25/sdPDO$devSD))
sd(newDat$recDevPDObc)

research1981 <- SS_output("C:/Users/r.wildermuth/Documents/FutureSeas/sardine_research_assessment")
resHistRec <- research1981$recruit %>% select(Yr, SpawnBio, exp_recr, pred_recr, dev, era) %>%
  mutate(scenario = "Hist1981")

plot(newDat$Year, newDat$recDevPDO, type = "l", 
     ylim = range(newDat$recDevSS_cyclPDO), xlim = c(1980, 2070),
     main = "Rec Devs from Cyclic PDO")
lines(resHistRec$Yr, resHistRec$dev, col = "coral2")
lines(newDat$Year, newDat$recDevPDObc, col = 4)
lines(newDat$Year, newDat$recDevSS_cyclPDO, col = 2)
abline(h = 0, col = "grey")
legend("topleft", legend = c("SShist", "cyclPDO0.78", "cyclPDObc", "cyclPDO1.25"),
       col = c("coral2", 1, 4,2), lty = 1, horiz = TRUE, cex = 0.7)
#write.csv(newDat, "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")

# Create recruitment deviation time series for PDO projection with climate change ------------

climPDO <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/ESMs_monthly_pdo_1976_2100.csv")

# Need to normalize the projected indexes per Mer Pozo in email "Environmental and recruitment data" 8/18/2022
sdClimPDO <- climPDO %>% summarize(gfdlPDO = sd(gfdl_pdo),
                                   ipslPDO = sd(ipsl_pdo),
                                   hadPDO = sd(had_pdo))
climPDO <- climPDO %>% mutate(gfdl_pdo = gfdl_pdo/sdClimPDO$gfdlPDO,
                              ipsl_pdo = ipsl_pdo/sdClimPDO$ipslPDO,
                              had_pdo = had_pdo/sdClimPDO$hadPDO) 
climPDO %>% mutate(yrmon = year + month/12) %>%
  ggplot(aes(x = yrmon, y = gfdl_pdo)) +
  geom_line()
# Check that ranges similar to Mantua data in 'moPDO'
climPDO %>% summarize(gfdlPDOmax = max(gfdl_pdo), gfdlPDOmin = min(gfdl_pdo),
                      ipslPDOmax = max(ipsl_pdo), ipslPDOmin = min(ipsl_pdo),
                      hadPDOmax = max(had_pdo), hadPDOmin = min(had_pdo))
moPDO %>% pivot_longer(c(janPDO, febPDO, marPDO, aprPDO, mayPDO, junPDO,
                         julPDO, augPDO, sepPDO, octPDO, novPDO, decPDO),
                       names_to = "monthPDO", values_to = "pdo") %>%
  summarize(moPDOmax = max(pdo), moPDOmin = min(pdo), moPDOsd = sd(pdo))

# Take mean of spring and summer seasons and sum as in Zwolinski & Demer 2014
climPDO <- climPDO %>% mutate(season = case_when(month %in% 3:7 ~ "spr",
                                      month %in% 8:10 ~ "sum",
                                      TRUE ~ "winter")) %>%
              group_by(season, year) %>%
              summarize(gfdl_pdo = mean(gfdl_pdo),
                        ipsl_pdo = mean(ipsl_pdo),
                        had_pdo = mean(had_pdo)) %>%
              # add spring and summer from previous year together (Zwolinski & Demer 2014)
              mutate(year = case_when(season == "sum" ~ year + 1,
                                      TRUE ~ year)) %>%
              group_by(year) %>% filter(season %in% c("spr", "sum")) %>%
              summarize(gfdl_pdo = sum(gfdl_pdo),
                        ipsl_pdo = sum(ipsl_pdo),
                        had_pdo = sum(had_pdo)) 

# Create ensemble mean for recruitment projection
climPDO$ensMeanPDO <- rowMeans(climPDO[,c("gfdl_pdo", "ipsl_pdo", "had_pdo")])

climPDO %>% full_join(y = newDat %>% select(Year, expPDO),
                      by = c("year" = "Year")) %>%
  pivot_longer(!year, names_to = "model", values_to = "pdo") %>%
  ggplot(aes(x = year, y = pdo, color = model)) +
  geom_line() 

# project rec devs using fit in Appendix A of Zwolinski & Demer 2019
# climPDO$recDev_GFDL <- MakeRecruitDevs(envtInx = climPDO$gfdl_pdo,
#                                     envtCoeff = 0.7815, 
#                                     devSD = 0.56) # pseudo-R^2 of PDO fit was 0.44 in Zwolinski & Demer 2019
# climPDO$recDev_IPSL <- MakeRecruitDevs(envtInx = climPDO$ipsl_pdo,
#                                        envtCoeff = 0.7815, 
#                                        devSD = 0.56) 
# climPDO$recDev_HAD <- MakeRecruitDevs(envtInx = climPDO$had_pdo,
#                                        envtCoeff = 0.7815, 
#                                        devSD = 0.56) 
climPDO <- climPDO %>% mutate(recDev_GFDL = (gfdl_pdo * 0.7815),
                              recDev_HAD = (had_pdo * 0.7815),
                              recDev_IPSL = (ipsl_pdo * 0.7815),
                              recDev_EMEAN = (ensMeanPDO * 0.7815))

# bias correction
sdPDO <- climPDO %>% filter(year >2019) %>% 
            summarize(devSD_GFDL = sd(recDev_GFDL),
                      devSD_HAD = sd(recDev_HAD),
                      devSD_IPSL = sd(recDev_IPSL),
                      devSD_EMEAN = sd(recDev_EMEAN))

# climPDO <- climPDO %>% mutate(recDev_GFDL = recDev_GFDL * (1.25/sdPDO$devSD_GFDL),
#                               recDev_HAD = recDev_HAD * (1.25/sdPDO$devSD_HAD),
#                               recDev_IPSL = recDev_IPSL * (1.25/sdPDO$devSD_IPSL))

climPDO %>% full_join(y = newDat %>% select(Year, recDevPDO),
                      by = c("year" = "Year")) %>%
  pivot_longer(c(recDev_GFDL, recDev_HAD, recDev_IPSL, recDev_EMEAN, recDevPDO), 
               names_to = "model", values_to = "pdo") %>%
  ggplot(aes(x = year, y = pdo, color = model)) +
  geom_line() 

# write.csv(climPDO, "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOclim2101.csv")

