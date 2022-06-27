#' Code to run harvest control rule 3 for sardine. This is applying a constant harvest rate of 0.18 (Emsy) to the biomass estimated from the EM
#' @param EM_out_dir is the path to the directory of the estimation model (EM, the simulated assessment)
#' @param EM_init_dir Initialization director that retains the reference files for interim assessments
#' @param init_loop Logical. If this is the first initialization loop of the MSE, \code{init_loop} should be TRUE. If it is in further loops, it should be FALSE.
#' @param OM_dat An valid SS data file read in using r4ss. In particular, this should be sampled data.
#' @param OM_out_dir The full path to the directory in which the OM is run.
#' @param OM_dat A valid SS data file read in using r4ss. In particular, this should be sampled data. 
#' @template verbose
#' @param sample_struct An optional list including which years and fleets should be
#'  added from the OM into the EM for different types of data. If NULL, the data
#'  structure will try to be infered from the pattern found for each of the
#'  datatypes within EM_datfile. Ignored if init_loop is TRUE.
#' @param interim_struct An optional including how many years to average over,
#'  fleet weights, the scaling rate (Beta) of catch relative to the index change for each fleet,
#'  and the reference year for each fleet (either a fixed year or <=0 relative to end_yr, fixed year
#'  will stay constant during simulation while relative year will progress with simulation).
#' @param seed to be specified in SS to generate the bootstrap
#' @param dat_yrs Which years should be added to the new model? Ignored if init_loop is TRUE.
#' @param nyrs_assess are the years between assessments
#' @return A list with a dataframe of catches by fleet and season to be input into the OM
#' @author Desiree Tommasi (code below based on parse_MS function developed by Kathryn Doering & Nathan Vaughan for SSMSE package)

MS_sar_hcr3 = function(EM_out_dir = NULL, 
                      init_loop = TRUE, OM_dat,
                      verbose = FALSE, nyrs_assess, dat_yrs,
                      sample_struct = NULL, interim_struct = NULL, seed = NULL, ...){
  library(dplyr)
  
  new_datfile_name <- "init_dat.ss"
  # change the name of data file.
  start <- r4ss:::SS_readstarter(file.path(EM_out_dir, "starter.ss"),
                                 verbose = FALSE
  )
  
  if (init_loop) {
    # copy over raw data file from the OM to EM folder
    r4ss:::SS_writedat(OM_dat,
                       file.path(EM_out_dir, new_datfile_name),
                       overwrite = TRUE,
                       verbose = FALSE
    )
    orig_datfile_name <- start[["datfile"]] # save the original data file name.
    start[["datfile"]] <- new_datfile_name
    start[["seed"]] <- seed
    r4ss:::SS_writestarter(start, file.path(EM_out_dir),
                           verbose = FALSE,
                           overwrite = TRUE, warn = FALSE
    )
    # make sure the data file has the correct formatting 
    new_EM_dat <- SSMSE:::change_dat(
      OM_datfile = new_datfile_name,
      EM_datfile = orig_datfile_name,
      EM_dir = EM_out_dir,
      do_checks = TRUE,
      verbose = verbose
    )
  } else {
    if (!is.null(sample_struct)) {
      sample_struct_sub <- lapply(sample_struct,
                                  function(df, y) df[df[, 1] %in% y, ],
                                  y = dat_yrs - nyrs_assess
      )
    } else {
      sample_struct_sub <- NULL
    }
    new_EM_dat <- SSMSE:::add_new_dat(
      OM_dat = OM_dat,
      EM_datfile = new_datfile_name,
      sample_struct = sample_struct_sub,
      EM_dir = EM_out_dir,
      nyrs_assess = nyrs_assess,
      do_checks = TRUE,
      new_datfile_name = new_datfile_name,
      verbose = verbose
    )
  }
  #Update SS random seed
  start <- r4ss:::SS_readstarter(file.path(EM_out_dir, "starter.ss"),
                                 verbose = FALSE
  )
  start[["seed"]] <- seed
  r4ss:::SS_writestarter(start, file.path(EM_out_dir),
                         verbose = FALSE,
                         overwrite = TRUE, warn = FALSE
  )
  # manipulate the forecasting file.
  # make sure enough yrs can be forecasted.
  fcast <- r4ss:::SS_readforecast(file.path(EM_out_dir, "forecast.ss"),
                                  readAll = TRUE,
                                  verbose = FALSE
  )
  # check that it can be used in the EM
  SSMSE:::check_EM_forecast(fcast,
                            n_flts_catch = length(which(new_EM_dat[["fleetinfo"]][, "type"] %in%
                                                          c(1, 2)))
  )
  
  mod_styr = new_EM_dat[["styr"]]
  mod_endyr = new_EM_dat[["endyr"]]
  
  #Change the years that are averaged over to calculate the quantities used to calculate the benchmark outputs.
  # 1 and 2 are the beginning and ending years for biology (e.g., growth, natural mortality, maturity, fecundity),
  # 3 and 4 are for selectivity
  # 5 and 6 for relative Fs
  # 7 and 8 for recruitment distribution
  # 9 and 10 for stock-recruitment parameters
  # In the sardine assessment case all are set to the end yr except for recruitment pars which start at the start of the acoustic survey
  fcast$Bmark_years=c(mod_endyr,mod_endyr,mod_endyr,mod_endyr,mod_endyr,mod_endyr,2005,mod_endyr,2005,mod_endyr)
  
  #Change first year for caps and allocations
  fcast$FirstYear_for_caps_and_allocations = mod_endyr + 2
  
  #change the year of the input apical F to be used in the forecast to the forecast year
  #in the actual assessment process the assessment is run once to find apical Fs for each fleet in the last year,
  #then the assessment is run a second time with the Fs for the forecast period set to the apical Fs
  #Here we assume the catches stay constant as those of the previous year
  #Specify that the forecast will specify future catches
  fcast$InputBasis=2
  #specify the forecast year
  fcast$ForeCatch$Year = rep((mod_endyr+1),6)
  #extract observed catch for last year of data
  fcatch = new_EM_dat$catch %>% dplyr::filter(year==mod_endyr)
  #specify the forecast catch as being the same as that in the latest observed year
  fcast$ForeCatch$`Catch or F`= fcatch$catch
  
  #save updated forecast file
  r4ss:::SS_writeforecast(fcast,
                          dir = EM_out_dir, writeAll = TRUE, overwrite = TRUE,
                          verbose = FALSE
  )
  
  # given all checks for the input files are good, run the EM
  SSMSE:::run_EM(EM_dir = EM_out_dir, verbose = verbose, check_converged = TRUE)
  
  #extract the required output, use EM_out_dir as input
  EM_out = r4ss:::SS_output(EM_out_dir, verbose = FALSE, printstats = FALSE, hidewarn = TRUE)
  
  #Extract the timeseries data for the forecast period (1 yr for sardine)
  EMts = EM_out$sprseries %>% dplyr::filter(Era=="FORE")
  
  #obtain the forecast Age 1+ biomass for the next year from the EM
  #as in the EM starter file the minimum age to calculate summary biomass is set at 1, Bio_Smry.1 can be used
  #the EM starter file specifies 1 forecast year, so only 1 value shoudl be available, but just in case specify first value is used
  bio1 = EMts$Bio_Smry.1[1]
  
  #set input to hcr
  Emsy = 0.18
  distribution = 0.87
  
  #This rule is the same as the adopted sardine rule except that there is no cutoff
  HG = (bio1)*Emsy*distribution
  
  #as for the regular sardine hcr, the hg is capped at a maximum catch of 200000 mt
  if (HG > 200000) {HG = 200000}
  #catches when hg is set to 0

  #catch ratio - based on average catch ratio from 2006-2011
  #This period was selected as allocation scheme changed in 2006. Also at the start of the time series
  #PNW states had no commercial permits for CPS, only experimental/developmental ones and thus PNW fleet was smaller
  #The catch ratios sharply changed to being more skewed to the PNW in season 1
  #when the summary biomass fell below ~400,000 mt in 2012 (likley due to very poor recruitment in 2011)
  #Fleet_Seas  Catch_ratio
  #MexCal_S1 1      0.0400
  #MexCal_S1 2      0
  #MexCal_S2 1      0
  #MexCal_S2 2      0.0838
  #PNW 1            0.830
  #PNW 2            0.0461
  #We could later run a scenario where the catch ratios change to the above when biomass is low. Intersting to see what the effect of
  #higher fishing pressure on older fish might be
  #The catch ratio we use (average 2006-2011). It is actually very similar to the 2001-2006 catch ratios (used 2001 as allocation scheme was different prior and federal management only instituted in 1998)
  #Fleet_Seas  Catch_ratio
  #MexCal_S1 1      0.225
  #MexCal_S1 2      0
  #MexCal_S2 1      0
  #MexCal_S2 2      0.326
  #PNW 1            0.435
  #PNW 2            0.0128
  
  cr_avg0611 = c(0.225,0,0,0.33,0.435,0.01)
  
  # catch_df = data.frame(
  #   year = rep((dat_yrs),6),
  #   seas = c(1,2,1,2,1,2),
  #   fleet = c(1,1,2,2,3,3),
  #   catch = c(2,2,2,2,2,2),
  #   catch_se = rep(0.01,6))
  
  
    catch_df = data.frame(
      year = rep((mod_endyr+1),6),
      seas = fcast$ForeCatch$Seas,
      fleet = fcast$ForeCatch$Fleet,
      catch = HG*cr_avg0611,
      catch_se = rep(new_EM_dat$catch$catch_se[1],6))
  
  #Set other inputs run_SSMSE will look for
  catch_bio = catch_df # catch in biomass. In this case, catch is in biomass for both. Could also be left as NULL
  catch_F = NULL # catch in terms of F, can be left as NULL.
  discards <- NULL # discards can be left as NULL since there are no discards
  #Put the new catches in the format needed by SSMSE according to parse_MS function
  catch_list <- list(catch = catch_df,
                     catch_bio = catch_bio, 
                     catch_F = catch_F,
                     discards = discards)
  
  return(catch_list)
  
}