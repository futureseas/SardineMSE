library(tidyverse)

source("R/MakeRecruitDevs.R")

moPDO <- read.csv("C:/Users/r.wildermuth/Documents/FutureSeas/Recruitment Index/monthlyPDO.csv")

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
newDat$recDevPDO <- MakeRecruitDevs(envtInx = newDat$expPDO,
                                    envtCoeff = 0.7815, 
                                    devSD = 0.56) # pseudo-R^2 of PDO fit was 0.44 in Zwolinski & Demer 2019

# bias correction
sdPDO <- newDat %>% summarize(devSD = sd(recDevPDO))

newDat <- newDat %>% mutate(recDevPDO = recDevPDO * (0.5/sdPDO$devSD))

#write.csv(newDat, "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOnoclim2120.csv")

# Create recruitment deviation time series for PDO projection with climate change ------------

climPDO <- read_csv("C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/ESMs_monthly_pdo_1976_2100.csv")


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

# project rec devs using fit in Appendix A of Zwolinski & Demer 2019
climPDO$recDev_GFDL <- MakeRecruitDevs(envtInx = climPDO$gfdl_pdo,
                                    envtCoeff = 0.7815, 
                                    devSD = 0.56) # pseudo-R^2 of PDO fit was 0.44 in Zwolinski & Demer 2019
climPDO$recDev_IPSL <- MakeRecruitDevs(envtInx = climPDO$ipsl_pdo,
                                       envtCoeff = 0.7815, 
                                       devSD = 0.56) 
climPDO$recDev_HAD <- MakeRecruitDevs(envtInx = climPDO$had_pdo,
                                       envtCoeff = 0.7815, 
                                       devSD = 0.56) 

# bias correction
sdPDO <- climPDO %>% filter(year >2019) %>% 
            summarize(devSD_GFDL = sd(recDev_GFDL),
                      devSD_HAD = sd(recDev_HAD),
                      devSD_IPSL = sd(recDev_IPSL))

climPDO <- climPDO %>% mutate(recDev_GFDL = recDev_GFDL * (0.5/sdPDO$devSD_GFDL),
                              recDev_HAD = recDev_HAD * (0.5/sdPDO$devSD_HAD),
                              recDev_IPSL = recDev_IPSL * (0.5/sdPDO$devSD_IPSL))

# write.csv(climPDO, "C:/Users/r.wildermuth/Documents/FutureSeas/SardineMSE/dat/recdevPDOclim2101.csv")
climPDO %>% ggplot(aes(x = year, y = gfdl_pdo)) + geom_line()
