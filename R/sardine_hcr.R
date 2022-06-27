#' Function implementing the PFMC harvest guideline for sardine
#' in the form of HG = (BIOMASS-CUTOFF)xFRACTIONxDISTRIBUTION
#' BIOMASS is the Age 1+ biomass forecast for that fishing year
#' FRACTION is the exploitation rate that leads to MSY. It is bound to be between 0.05 and 0.2
#' When not enviro correlated it is taken as 0.18 from the PFMC Hurtado-Ferro/Punt MSE for PFMC
#' Note that analysis was conducted using M=0.4, and Ricker recruitment
#' BIOMASS and Emsy are obtained from the EM output file
#'
#' @param EM_out is the output from the estimation model (EM, the simulated assessment) as read by SS_output 
#' @param cutoff is the biomass below which the fishery is closed. This is 150000 mt in the current PFMC hcr
#' @param Emsy is the exploitation rate that would lead to MSY. In the current PFMC hcr this is environmentally informed. It is bound to be between 0.05 and 0.2
#' @param distirbution specifies how much of the biomass is in US waters. Currently set to 0.87
#' @param maxcatch sets the maximum possible HG in mt
#' @return A harvest guideline in mt
#' @author Desiree Tommasi

sardine_hcr = function(EM_out, cutoff, Emsy, distribution, maxcatch){
  
  #Extract the timeseries data for the forecast period (1 yr for sardine)
  EMts = EM_out$sprseries %>% filter(Era=="FORE")
  
  #obtain the forecast Age 1+ biomass for the next year from the EM
  #as in the EM starter file the minimum age to calculate summary biomass is set at 1, Bio_Smry.1 can be used
  #the EM starter file specifies 1 forecast year, so only 1 value shoudl be available, but just in case specify first value is used
  bio1 = EMts$Bio_Smry.1[1]
  
  #make sure if Emsy is between 0.05 and 0.20 
  if (Emsy > 0.20) {Emsy = 0.20} else if (Emsy < 0.05) {Emsy = 0.05}  
  
  #if biomass is less than the cutoff, the harvest guideline is set to 0, if not the current hg rule is ised
  #HG=(BIOMASS-CUTOFF)xFRACTIONxDISTRIBUTION
  if (bio1 < cutoff) {HG = 0} else {HG = (bio1-cutoff)*Emsy*distribution}
  
  #the hg is capped at a maximum catch of 200000 mt
  if (HG > 200000) {HG = 200000}
  
  return(HG)
}