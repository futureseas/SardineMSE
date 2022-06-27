# Use SSMSE to perform MSE loop using Peter Kuriyama's sardine research SS model as operating model and 
# a constant growth assessment SS model.
# Created: 6/15/2021, Robert Wildermuth

library(tidyverse)
library(SSMSE)
library(r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE

# directory for MSE output
mseOutputPath <- "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/researchOM_constantgrowthEM"

# Operating Model - Research Model ----------------------------------------

# directory for OM SS code
OMmodelPath <- "C:/Users/rwildermuth/Documents/FutureSeas/testNew"
# RW: need to re-save data.ss_new as data.ss to fix formatting for SS_readdat()

# ----- Look at forecasting ---- 
fore <- r4ss::SS_readforecast(file = paste0(OMmodelPath, "/forecast.ss"),  verbose = FALSE)
fore$Forecast
fore$Btarget
fore$BforconstantF
fore$BfornoF
fore$Flimitfraction
fore$Nforecastyrs


# Define Observation Model ------------------------------------------------
datfile <- SS_readdat(file = paste0(OMmodelPath, "/data.ss"), version = "3.30")
sample_struct <- create_sample_struct(dat = datfile, nyrs = 6)

# create_sample_strct() has trouble IDing SE for survey CPUE
# define an index for the Acoustic-Trawl survey as in Desiree's code
#specify number of years of MSE loop
nyrs = 6

#specify the start year of data inputs
yrsrt = datfile$endyr +1

#specify the end year of data inputs
yrend = datfile$endyr + nyrs

sample_struct$CPUE = sample_struct$CPUE[1:nyrs,]
sample_struct$CPUE$FltSvy = 4
sample_struct$CPUE$SE = 0.5
sample_struct$CPUE$Yr= yrsrt:yrend
sample_struct$CPUE$Seas= 1

# for now assume no additional age or length comps
sample_struct$lencomp <- NULL
sample_struct$agecomp <- NULL

sample_struct_list <- list("researchModel" = sample_struct)
# Run the OM --------------------------------------------------------------

#run_res_path <- file.path("C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE", "results")
# dir.create(mseOutputPath)

EMmodelPath <- "C:/Users/rwildermuth/Documents/FutureSeas/test2"
# EM starter.ss file must indicate init values are to be pulled from control.ss file, not ss.par
  
run_SSMSE(scen_name_vec = "researchModel",# name of the scenario
          out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
          iter_vec = c(2), # run with 2 iterations for now
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = OMmodelPath, # OM files
          EM_name_vec = "researchOM_constantgrowthEM", # cod is included in package data
          EM_in_dir_vec = EMmodelPath, # EM files
          MS_vec = "EM",       # The management strategy is specified in the EM
          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
          nyrs_vec = 2,        # Years to project OM forward
          nyrs_assess_vec = 1, # Years between assessments
          rec_dev_pattern = "rand", # Use random recruitment devs
          scope = "2", # to use the same recruitment devs across scenarios.
          impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = TRUE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 12345) #Set a fixed integer seed that allows replication


# Summarize results -------------------------------------------------------

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(mseOutputPath)

sumry$ts[, c("model_run", "year", "SpawnBio", "iteration")]
# Get rid of duplicated SSB years
tsMod <- na.omit(sumry$ts)

ggplot2::ggplot(data = subset(tsMod, model_run %in% c("testNew_OM", 
                                                         "researchOM_constantgrowthEM_EM_2020")), 
                ggplot2::aes(x = year, y = SpawnBio)) +
  ggplot2::geom_vline(xintercept = 2019, color = "gray") +
  ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run))+
  ggplot2::scale_color_manual(values = c("#D65F00", "black", "blue")) +
  ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
  ggplot2::guides(linetype = FALSE) +
  ggplot2::facet_wrap(. ~ scenario) +
  ggplot2::theme_classic()

# The get_rel_SSB_avg calculates the relative SSB in each year for each
# iteration of the operating model, then takes the average over the years from
# min_yr, to max_year. It uses the summary object as input to do these
# calculations.
get_rel_SSB_avg <- function(scalarSmry, tsSmry, min_yr, max_yr) {
  # Get just the result for the OMs and not for the EMs.
  OM_vals <- unique(tsSmry$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  # find the unfished biomass fr the OMs
  B_unfished <- scalarSmry %>% 
    filter(model_run %in% OM_vals) %>% 
    select(iteration, scenario,SSB_Unfished)
  #  find the spawning stock biomass for the years of interest
  SSB_yr <- tsSmry %>% 
    filter(year >= min_yr & year <= max_yr) %>% 
    select(iteration, scenario, year, SpawnBio)
  # Calculated the relative spawning stock biomass using B_unfished and SSB_yr
  # dataframes, then take an average over years.
  SSB_yr <- left_join(SSB_yr, B_unfished) %>% 
    mutate(Rel_SSB = SpawnBio/SSB_Unfished) %>% 
    group_by(iteration, scenario) %>% 
    summarize(avg_SSB = mean(Rel_SSB), .groups = "keep") %>% 
    ungroup()
  SSB_yr # return SSB averaged over yrs for each iteration and each scenario.
}
rel_SSB <- get_rel_SSB_avg(scalarSmry = sumry$scalar, tsSmry = tsMod, min_yr = 2020, max_yr = 2021)
## Joining, by = c("iteration", "scenario")

# function to summarize data in plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}
# Now, plot the average relative spawning stock biomass for years 104 - 106
ggplot(data = rel_SSB, aes(x = scenario, y = avg_SSB)) +
  geom_hline(yintercept = 0.4, color = "gray") +
  stat_summary(fun.data = data_summary, 
               position = position_dodge(width = 0.9), color = "blue") +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(title = "Long-term average relative SSB\n(years 2020-2021)", 
       x = "Scenario", y = "SSB/SSB_unfished") +
  theme_classic()
