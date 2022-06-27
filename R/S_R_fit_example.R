#Example of fitting a Beverton Holt S-R using negative log likelihood on simulated data and output from S. Koenigstein MICE model
#D.Tommasi, 02-15-2022
library(r4ss)

#Assuming SSB is the same as for OM_K
omk="/home/desiree/Documents/COCA/Sardine/Sardine_MSE/OM_K"
omkdat=SS_output(omk)

sprt=omkdat$sprseries %>% filter(Era=="TIME")
ssb=sprt$SSB
lnr0=15.3516 #from control file
h=0.5 #from control file
sb0=6.51485e+05 #virgin ssb from derived quantities

#we use the common BH formulation, so we need to calculate a and b from the we use as the SS quantities
b=sb0*(1-h)/(5*h-1)
a=(4*h*exp(lnr0)/(5*h-1))

#Beverton Holt function
BH_lin = function(pars,sb) {
  a=pars[1]
  b=pars[2]
  recl=log(a*sb/(b+sb))
  return(recl)
}

#Generate some recruits using the function and some randomly generated rec devs
set.seed(1234)
rdev=rnorm(39)
rec= exp(BH_lin(c(a, b), ssb)+rdev)

#negative log likelihood function. We assume the sigma is the same as the stock assessment, 1
dat=data.frame(ssb=ssb,rec=rec)
nll = function(pars, data) {
  # Values predicted by the model
  Rpred = BH_lin(pars, data$ssb)
  Robs = log(data$rec)
  # Negative log-likelihood 
  -sum(dnorm(x = Robs, mean = Rpred, sd = 1, log = TRUE))
}

#use optim to estimate the parameters - choosing initial values that are pretty far from actual
fit = optim(par = c(1000,10), fn = nll, method = "L-BFGS-B",
            lower = rep(2, 1), data = dat)

#check fit to the simulated rec data
plot(ssb,log(rec))
curve(BH_lin(fit$par, x), 0, 5e+7,n=100000, add = TRUE,col="green")

#Now use the MICE output data instead

#code from Robert ExploreRecruitIndices.R to calculate SSB from the MICE numbers at age and obtain a timeseries of recs and ssb
wtAgeDat <- SS_readwtatage(file="/home/desiree/Documents/COCA/Sardine/sardine2020base/wtatage.ss")
test1 <- wtAgeDat %>% filter(Fleet == 4, Seas == 2)

srMICE <- read_csv(file = "/home/desiree/Documents/COCA/Sardine/MICE stock-recruits.csv")
#srMICE <- read_csv(file = "/home/desiree/Documents/COCA/Sardine/MICE ensemble recruits2.csv")
srMICE <- srMICE %>% mutate(mean_k = mean/1000, # get numbers in 1000s to match SS output
                            min_k = min/1000,
                            max_k = max/1000,
                            meanELSdriven_k = meanELSdriven/1000)
srMICE <- srMICE %>% mutate(mean_k = mean/1000 # get numbers in 1000s to match SS output
                            )

#plot the data
ggplot(srMICE, aes(y=adult.stock.sum, x=as.factor(configuration))) + 
  geom_violin()

srMICE70 = srMICE %>% filter(year>2070)
srMICE30 = srMICE %>% filter(year>2030&year<2071)

e70=ecdf(srMICE70$adult.stock.sum)
e70(2e+10)

e30=ecdf(srMICE30$adult.stock.sum)
e30(2e+10)

#meanELSdriven_k = meanELSdriven/1000)
test2 <- srMICE %>% filter(ModelParam %in% c("adult.stock.sum", "recruits")) %>%
  pivot_wider(id_cols = c(year, GCM), names_from = ModelParam, values_from = mean)

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
matAtAge <- read_csv("/home/desiree/Documents/COCA/Sardine/maturityAtAge_02092022.csv")
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
         meanELSBiom_kg = mean * indivWtAvg * PropMature)

recs <- srMICE %>% filter(ModelParam == "recruits")

ssb <- srMICE %>% filter(ModelParam != "recruits") %>%
  group_by(year, GCM) %>%
  summarize(meanSSB = sum(meanBiom_kg) * 0.001, # convert to metric tons
            meanELSSSB = sum(meanELSBiom_kg) * 0.001) %>%
  mutate(yearPlus1 = year + 1)

ssbrecsMICE <- ssb %>% left_join(y = recs, by = c("GCM", "yearPlus1" = "year"), keep = TRUE)

#organize data in the same data frame format as in the example
#Modify the nll fun to use a sigma of 1.25 instead of 1 as the 2001 OM

nll01 = function(pars, data) {
  # Values predicted by the model
  Rpred = BH_lin(pars, data$ssb)
  Robs = log(data$rec)
  # Negative log-likelihood 
  -sum(dnorm(x = Robs, mean = Rpred, sd = 1.25, log = TRUE))
}

ssbh=ssb %>% filter(GCM=="RA")
ssbp=ssb %>% filter(GCM!="RA")
recsh=recs %>% filter(GCM == "RA"&ModelParam=="recruits")
recsp=recs %>% filter(GCM != "RA"&ModelParam=="recruits")
MICEdat=data.frame(ssb=ssbh$meanSSB,rec=recsh$mean)

plot(MICEdat$ssb,log(MICEdat$rec))

MICEfit = optim(par = c(1000,10), fn = nll01, method = "L-BFGS-B",
            lower = rep(2, 1), data = MICEdat)

#check fit to the simulated rec data
plot(MICEdat$ssb,log(MICEdat$rec),ylim=c(10,24))
curve(BH_lin(MICEfit$par, x), 0, 5e+7,n=100000, add = TRUE,col="green")
curve(BH_lin(c(a,b), x), 0, 5e+7,n=100000, add = TRUE,col="blue")#note this is using the S_R param from the 1980 OM_K, can add 2001 OM curve

#calculate the rec devs
MICEdevs=log(MICEdat$rec)-BH_lin(MICEfit$par, MICEdat$ssb)
MICEdevsall=log(recs$mean)-BH_lin(MICEfit$par, ssb$meanSSB)

#add the projected rec devs to the plot
plot(MICEdat$ssb,log(MICEdat$rec),ylim=c(10,24))
curve(BH_lin(MICEfit$par, x), 0, 5e+7,n=100000, add = TRUE,col="green")
curve(BH_lin(c(a,b), x), 0, 5e+7,n=100000, add = TRUE,col="blue")#note this is using the S_R param from the 1980 OM_K, can add 2001 OM curve
points(ssbp$meanSSB,log(recsp$mean),col="red") #interesting the projected rec devs are quite similar to the historical

#plot time series of rec devs
ssb$rdevs=MICEdevsall
pdat=as.data.frame(ssb)

ggplot(data=pdat,aes(x = year, y = rdevs,group=as.factor(GCM),color=as.factor(GCM))) +
  geom_line()

#bias correct difference in variance
pdat$rdevsbc=pdat$rdevs*(1.25/0.52)
ggplot(data=pdat,aes(x = year, y = rdevsbc,group=as.factor(GCM),color=as.factor(GCM))) +
  geom_line()+facet_wrap(.~GCM)

CAFAdat=pdat%>%filter(GCM=="GFDL"&year>2020)
cplot=ggplot(data=CAFAdat,aes(x = year, y = rdevsbc)) +
  geom_line()+ylab("Recrutiment Deviations")+xlab("")

setwd("/home/desiree/Documents/COCA/Sardine/")
dev.new(width=6, height=5)
tiff("CAFAplot.tiff", width = 6, height = 5, units = 'in', res = 300, compression = 'lzw')
cplot
dev.off()

#Assume the MICE provides 0.7 of the variance and 0.3 is random 
rndmat=data.frame(year=c(1980:2018,2020:2100),rndmr=rnorm(120,0,1.25))
pdatr=merge(pdat,rndmat)
pdatr$rdevs_plusr=pdatr$rdevsbc*0.7+0.3*pdatr$rndmr
ggplot(data=pdatr,aes(x = year, y = rdevs_plusr,group=as.factor(GCM),color=as.factor(GCM))) +
  geom_line()+facet_wrap(.~GCM)
