# Check catch limits and thresholds for HCR5


EM_out_dir <- "C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrowthShortOMandEM_RandRec_HCR5/2/testHCR5_EM_2058"
EM_out = r4ss:::SS_output(EM_out_dir, verbose = FALSE, printstats = FALSE, hidewarn = TRUE)

EMts = EM_out$sprseries %>% dplyr::filter(Era=="FORE")

#obtain the forecast Age 1+ biomass for the next year from the EM
#as in the EM starter file the minimum age to calculate summary biomass is set at 1, Bio_Smry.1 can be used
#the EM starter file specifies 1 forecast year, so only 1 value shoudl be available, but just in case specify first value is used
bio1 = EMts$Bio_Smry.1[1]

#set input to hcr
Emsy = 0.18
distribution = 0.87

#Extract unfished SSB and compute reference points
#Note that SS does not output dynamic B0 only SSB0, so even for equilibirum SSB0 Ftarget will be modified based on SSB0 rather than total B0
B0=EM_out$derived_quants$Value[EM_out$derived_quants$Label=="SSB_unfished"]
B040=0.4*B0
B080=0.8*B0

#Exract current SSB
Bcur=EM_out$Dynamic_Bzero$SSB[EM_out$Dynamic_Bzero$Yr==2059]

#Compute F target level
if (Bcur > B080) {Ftar = Emsy } else if (Bcur < B080 & Bcur >B040)
{Ftar = (Emsy/(B080-B040))*(Bcur-B040)} else {Ftar = 0}

#biomass is computed according to the HG rule except the fraction is dependent on SSB relative to reference points and there is no cutoff (but HG is 0 if Bcur is below Blim as Ftar is 0)
HG = (bio1)*Ftar*distribution

#as for the regular sardine hcr, the hg is capped at a maximum catch of 200000 mt
if (HG > 200000) {HG = 200000}

# to be above catch limit, 'bio1' must be greater than:
200000/(Ftar*distribution)

# Check the OM
omDat <- SS_readdat(file = file.path("C:/Users/r.wildermuth/Documents/FutureSeas/SardineScenarios/constGrowthShortOMandEM_RandRec_HCR5/2/start2001_OM", "data.ss_new"),
                      version = "3.30", verbose = FALSE)
omDat$catch %>% #filter(year >= 2058) %>% 
  group_by(year) %>% summarize(totCatch = sum(catch)) %>% pull(totCatch)
