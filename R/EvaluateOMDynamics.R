library(r4ss)

omDir <- "0_estimate_cohort_growh"

#------------------------------------------------------------------
# look at the data provided

omDat <- SS_readdat(paste0(omDir, "/data.ss"))


#Making plots
# pull in output
cohort_growth <- SS_output(dir = omDir, repfile = "Report.sso", printstats = FALSE)
cohort_growth$timeseries
head(cohort_growth$parameters )

# save plot info
SS_plots(cohort_growth)

# tv_growth <- SS_output("final_tvgrowth_may2021", printstats = FALSE)
# SS_plots(tv_growth)
# 
# #Plot comparisons
# two_mods <- list(cohort_growth = cohort_growth, tv_growth = tv_growth)
# two_mods_summ <- SSsummarize(two_mods)
# two_mods_summ
# 
# SSplotComparisons(two_mods_summ, plotdir = "demo_plots", print = T)