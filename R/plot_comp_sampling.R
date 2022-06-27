# RW: New plot_comp_sampling() to handle different naming on scenarios

#' Plot comp data, expected values, and sampled data for 1 scenario
#'
#' Creates a plot that can be used to see how sampling lines up with 
#' data and expected values for the index of abundance
#' @param dir Path to the directory containing 1 scenario. Defaults to
#'  the current working directory.
#' @param comp_type Type of composition data, age or length. Defaults to age.
#' @export
#' @import ggplot2
#' @author Kathryn Doering
#' @return A list containing 2 components: 1) the ggplot object and 2) the
#'  dataframe used to make the ggplot object

dir = "C:/Users/rwildermuth/Documents/FutureSeas/SardineMSE/cohortGrowthOMandEM/cohortGrowthselftest"
comp_type = "lencomp"

plot_comp_sampling <- function(dir = getwd(), comp_type = c("agecomp", "lencomp")) {
  
  comp_type <- match.arg(as.character(comp_type), choices = c("agecomp", "lencomp"))
  # get the iterations
  iters <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  scenario <- basename(dir)
  # get the OM data values
  om_name <- grep("_OM", list.dirs(file.path(dir, iters[1]),
                                  recursive = FALSE,
                                  full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(om_name)
  # non-NULL compfile input provided and file exists
  out_OM <- r4ss::SS_output(file.path(dir, as.character(iters[1]),
                                      om_name),
                            verbose = FALSE, 
                            printstats = FALSE, 
                            hidewarn = TRUE)
  
  if(comp_type == "agecomp") comp_dbase <- out_OM[["agedbase"]]
  if(comp_type == "lencomp") comp_dbase <- out_OM[["lendbase"]]
  if(isTRUE(nrow(comp_dbase) == 0)) {
    stop("The comp database from the operating model has no rows, so must not ", 
         "have been any historical data in the OM.")
  }
  comp_dbase <- type.convert(comp_dbase)
  comp_dbase[["iteration"]] <- 1
  comp_dbase[["scenario"]] <- scenario
  comp_dbase[["model_run"]] <- "om"
  # Get the EM data ----
  # get the EM init values
  # em name is the same across iterations
  em_name <- grep("EM_init$", list.dirs(file.path(dir, iters[1]),
                                        recursive = FALSE,
                                        full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(em_name)
  for (i in iters) {
    tmp_out_EM <- r4ss::SS_output(file.path( dir, as.character(i),
                                             em_name),
                                  verbose = FALSE, 
                                  printstats = FALSE, 
                                  hidewarn = TRUE)
    if(comp_type == "agecomp") tmp_comp_dbase <- tmp_out_EM[["agedbase"]]
    if(comp_type == "lencomp") tmp_comp_dbase <- tmp_out_EM[["lendbase"]]
    
    tmp_comp_dbase[["iteration"]] <- i
    tmp_comp_dbase[["scenario"]] <- scenario
    tmp_comp_dbase[["model_run"]] <- "em"
    comp_dbase <- rbind(comp_dbase, tmp_comp_dbase)
  }
  # get expected and observations in the same column
  comp_dbase <- tidyr::gather(comp_dbase, "type_obs", "obs_value", 17:18) %>% 
    dplyr::filter(model_run == "om" | (model_run == "em" & type_obs == "Obs"))
  comp_dbase[["model_type_obs"]] <- paste0(comp_dbase[["model_run"]], "_", 
                                           comp_dbase[["type_obs"]])
  comp_dbase <- tidyr::spread(comp_dbase, model_type_obs, obs_value) %>% 
    dplyr::mutate(Yr_lab = paste0("Yr: ", Yr)) %>%
    dplyr::mutate(Seas_lab = paste0("Seas: ", Seas)) %>% 
    dplyr::mutate(Sex_lab = paste0("Sex: ", Sex))
  # Make the plot ----
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning(
      "The package ggplot2 is not available.",
      " Returning early with just the dataframe and not the plot."
    )
    return(list(comp_dat = comp_dbase, plot = NA))
  }
  if(comp_type == "agecomp") xlab_val <- "Age"
  if(comp_type == "lencomp") xlab_val <- "Size Bins"
  # need a loop for multiple fleets (I think we just want a plot per fleet)
  # Need to add in better labels for sex
  comp_plot <- vector(mode = "list", length = length(unique(comp_dbase[["Fleet"]])))
  for(f in unique(comp_dbase[["Fleet"]])) {
    ind <- which(unique(comp_dbase[["Fleet"]]) == f)
    tmp_dbase_subset <- comp_dbase[comp_dbase[["Fleet"]] == f, ]
    comp_plot[[ind]] <-  ggplot(tmp_dbase_subset, aes(x = Bin, y = om_Exp)) +
      geom_area(fill = "grey")+
      geom_point(aes(y = om_Obs), color = "red", size = 2)+
      geom_point(aes(y = em_Obs, shape = iteration), color = "black")+
      facet_wrap(vars(Yr_lab, Seas_lab, Sex_lab))+
      scale_shape_manual(values = 
                           rep(15, length(unique(comp_dbase[["iteration"]]))))+
      ylab("Proportion")+
      xlab(xlab_val)+
      ggtitle(paste0("Fleet ", f ))+
      theme_classic()
  }
  
  comp_list <- list(comp_dat = comp_dbase, plot = comp_plot)
}