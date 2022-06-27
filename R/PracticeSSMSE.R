# Practice using SSMSE with Peter Kuriyama's sardine SS models
# Created: 6/7/2021, Robert Wildermuth

library(tidyverse)
library(SSMSE)
library(r4ss)

# directory for MSE output
mseOutputPath <- "C:/Users/Robert W/Documents/FutureSeas/SardineMSE/constGrowthOMandEM"

# Operating Model - Research Model ----------------------------------------

# directory for OM SS code
OMmodelPath <- "C:/Users/Robert W/Documents/FutureSeas/SardineMSE/constGrowth_test2"

# Create the OM - This modifies and re-runs the OM. Not needed now
# develop_OMs(OM_in_dir = OMmodelPath, out_dir = mseOutputPath,
#             # keep same steepness value for S-R relationship (in the research assessment control file INIT column)
#             par_name = "SR_BH_steep", par_vals = 0.5,
#             refit_OMs = FALSE, hess = FALSE)
# # ---- troubleshooting develop_OMs() -----
# traceback()
# 
# # code from develop_OMs()
# OM_in_dir = OMmodelPath
# out_dir = mseOutputPath
# par_name = "SR_BH_steep"
# par_vals = 0.5
# refit_OMs = FALSE
# hess = FALSE
# OM_name = NULL
# 
# # check input
# if (!is.null(OM_name)) assertive.types::assert_is_a_string(OM_name)
# if (!is.null(OM_in_dir)) assertive.types::assert_is_a_string(OM_in_dir)
# assertive.types::assert_is_a_string(out_dir)
# assertive.types::assert_is_a_string(par_name)
# assertive.properties::assert_is_atomic(par_vals)
# assertive.types::assert_is_a_bool(refit_OMs)
# if (is.null(OM_name) & is.null(OM_in_dir)) {
#   stop(
#     "OM_name and OM_in_dir are both NULL. Please specify an OM_name or ",
#     "OM_in_dir."
#   )
# }
# # get the path to the OM if it is in the external package data.
# # specify the OM_in_dir if only specified OM by name.
# if (!is.null(OM_name)) {
#   pkg_dirs <- list.dirs(system.file("extdata", "models", package = "SSMSE"))
#   pkg_dirs <- pkg_dirs[-grep("models$", pkg_dirs)] # git rid of model directory.
#   OM_in_dir <- pkg_dirs[grep(OM_name, pkg_dirs)]
#   if (length(OM_in_dir) != 1) {
#     stop(
#       "OM_name ", OM_name, " matched ", length(OM_in_dir), " models in ",
#       "SSMSE external package data, but should match 1. Please ",
#       "change OM_name to match (or partially match unambiguously) with 1 ",
#       "model in the models folder of the SSMSE external package data. ",
#       "Model options are: ", paste0(basename(pkg_dirs), collapse = ", ")
#     )
#   }
# }
# # read in the starterfile, b/c should be the same across iterations
# start <- r4ss::SS_readstarter(
#   file = file.path(OM_in_dir, "starter.ss"),
#   verbose = FALSE
# )
# if (refit_OMs == FALSE) {
#   # warning("Parameter devs will all be 0 in the OM if the model is not refit.",
#   #         " If parameter devs are desired, use refit_OMs = TRUE.")
#   # read in parfile to save the recdevs.
#   parfile <- r4ss::SS_readpar_3.30(
#     parfile = file.path(OM_in_dir, "ss.par"),
#     datsource = file.path(OM_in_dir, start[["datfile"]]),
#     ctlsource = file.path(OM_in_dir, start[["ctlfile"]]),
#     verbose = FALSE
#   )
#   # note: may want to save  forecast recdevs also?
# }
# # !!!:  Error in FUN(newX[, i], ...) : invalid 'type' (character) of argument
# 
# # code for SS_readpar_3.30()
# parfile = file.path(OM_in_dir, "ss.par")
# datsource = file.path(OM_in_dir, start[["datfile"]])
# ctlsource = file.path(OM_in_dir, start[["ctlfile"]])
# verbose = FALSE
# 
# if (is.character(datsource)) {
#   datlist <- SS_readdat(file = datsource, version = "3.30", 
#                         verbose = FALSE)
# }
# # !!!: Error in FUN(newX[, i], ...) : invalid 'type' (character) of argument
# 
# datlist <- SS_readdat(file = datsource, version = "3.30", 
#                       verbose = TRUE, echoall = TRUE)
# 
# # code for SS_readdat
# file = datsource
# version = "3.30"
# verbose = FALSE
# echoall = FALSE 
# section = NULL
# 
# if (is.null(version)) {
#   version <- scan(file, what = character(), nlines = 5, 
#                   quiet = !verbose)
#   version <- substring(version, 3, 6)
#   version <- version[version %in% c("3.24", "3.30")]
#   if (length(version) > 0) {
#     if (verbose) 
#       cat("assuming version", version, "based on first five lines of data file\n")
#   }
#   else {
#     newfile <- file.path(dirname(file), "data.ss_new")
#     if (file.exists(newfile)) {
#       version <- scan(newfile, what = character(), 
#                       nlines = 1, quiet = !verbose)
#       version <- substring(version, 3, 6)
#       if (verbose) 
#         cat("assuming version", version, "based on first line of data.ss_new\n")
#     }
#     else {
#       stop("input 'version' required due to missing value at top of", 
#            file)
#     }
#   }
# }
# nver <- as.numeric(substring(version, 1, 4))
# if (verbose) 
#   cat("Char version is ", version, "\n")
# if (verbose) 
#   cat("Numeric version is ", nver, "\n")
# if (nver < 3) {
#   datlist <- SS_readdat_2.00(file = file, verbose = verbose, 
#                              echoall = echoall, section = section)
# }
# if ((nver >= 3) && (nver < 3.2)) {
#   datlist <- SS_readdat_3.00(file = file, verbose = verbose, 
#                              echoall = echoall, section = section)
# }
# if ((nver >= 3.2) && (nver < 3.3)) {
#   datlist <- SS_readdat_3.24(file = file, verbose = verbose, 
#                              echoall = echoall, section = section)
# }
# if (nver >= 3.3) {
#   datlist <- SS_readdat_3.30(file = file, verbose = verbose, 
#                              echoall = echoall, section = section)
# }
# #!!!: Error in FUN(newX[, i], ...) : invalid 'type' (character) of argument
# 
# traceback()
# # apply(datlist[["lencomp"]][, -(1:6)], MARGIN = 1, FUN = sum)
# 
# # code for SS_readdat_3.30()
# if (verbose) {
#   message("Running SS_readdat_3.30")
# }
# if (echoall) {
#   message("Echoing blocks of data as it's being read")
#   if (!verbose) {
#     message("Changing input 'verbose' to TRUE")
#     verbose <- TRUE
#   }
# }
# dat <- readLines(file, warn = FALSE)
# if (length(dat) < 20) {
#   warning("Data file appears to be empty or incomplete.\n", 
#           "  If this is data.ss_new, change starter file to have\n", 
#           "  nonzero value for 'Number of datafiles to produce'")
#   return()
# }
# Comments <- get_comments(dat)
# # !!!: Error in if (!(names(filter) %in% c("parent_id", "comment_id"))) { : 
# #       argument is of length zero
# Comments <- r4ss::get_comments(dat)
# # !!!: Error: 'get_comments' is not an exported object from 'namespace:r4ss'
# 
# sec.end.inds <- grep("^999\\b", dat)
# incorr.secs <- NULL
# for (i in 1:length(sec.end.inds)) {
#   check_section <- dat[sec.end.inds[i]]
#   check_section <- strsplit(check_section, "#")[[1]][1]
#   check_section <- unlist(strsplit(unlist(strsplit(check_section, 
#                                                    "\t")), " "))
#   check_section <- check_section[check_section != ""]
#   if (length(check_section) > 1) {
#     incorr.secs <- c(incorr.secs, i)
#   }
# }
# if (length(incorr.secs) > 0) {
#   sec.end.inds <- sec.end.inds[-incorr.secs]
# }
# Nsections <- length(sec.end.inds)
# if (!Nsections) {
#   stop("Error - There was no EOF marker (999) in the data file.")
# }
# if (is.null(section)) {
#   if (Nsections > 1 & verbose) {
#     message("The supplied data file has ", Nsections, 
#             ifelse(Nsections == 1, " section. ", " sections. "), 
#             " Using section = 1.")
#   }
#   section <- 1
# }
# if (!section %in% 1:Nsections) {
#   if (Nsections == 1) {
#     stop("The 'section' input must be 1 for this data file.\n")
#   }
#   else {
#     stop("The 'section' input must be between 1 and ", 
#          Nsections, " for this data file.\n")
#   }
# }
# if (!is.null(section)) {
#   start <- 1
#   end <- sec.end.inds[section]
#   if (section > 1) {
#     start <- sec.end.inds[section - 1] + 1
#   }
#   dat <- dat[start:end]
# }
# find.index <- function(dat, ind, str) {
#   while (ind < length(dat) & !length(grep(str, dat[ind]))) {
#     ind <- ind + 1
#   }
#   if (ind == length(dat)) {
#     stop("SS_readdat_3.30-find.index: Error - ", 
#          "the value of ", str, " was not found. ", 
#          "Check the data file and make sure all ", 
#          "data frames are correctly formed.\n")
#   }
#   ind
# }
# get.val <- function(dat, ind) {
#   assign("ind", ind + 1, parent.frame())
#   as.numeric(dat[ind])
# }
# get.vec <- function(dat, ind) {
#   assign("ind", ind + 1, parent.frame())
#   vec <- strsplit(dat[ind], "[[:blank:]]+")
#   as.numeric(vec[[1]])
# }
# get.df <- function(dat, ind, nrow = NULL) {
#   if (is.null(nrow)) {
#     end.ind <- find.index(dat, ind, "-9999")
#     assign("ind", end.ind + 1, parent.frame())
#     if (ind != end.ind) {
#       df <- dat[ind:(end.ind - 1)]
#     }
#     else {
#       return(NULL)
#     }
#   }
#   else {
#     df <- dat[ind:(ind + nrow - 1)]
#     assign("ind", ind + nrow, parent.frame())
#   }
#   df <- strsplit(df, "[[:blank:]]+")
#   df <- as.list(df)
#   df <- do.call("rbind", df)
#   df <- as.data.frame(df, stringsAsFactors = FALSE)
#   df <- utils::type.convert(df, as.is = TRUE)
#   return(df)
# }
# dat <- gsub("^[[:blank:]]+", "", dat)
# dat <- gsub("#.*", "", dat)
# dat <- gsub("[[:blank:]]+$", "", dat)
# dat <- dat[dat != ""]
# datlist <- list()
# datlist[["sourcefile"]] <- file
# datlist[["type"]] <- "Stock_Synthesis_data_file"
# datlist[["ReadVersion"]] <- "3.30"
# if (verbose) {
#   message("SS_readdat_3.30 - read version = ", datlist[["ReadVersion"]])
# }
# datlist[["Comments"]] <- Comments
# ind <- 1
# datlist[["styr"]] <- get.val(dat, ind)
# datlist[["endyr"]] <- get.val(dat, ind)
# datlist[["nseas"]] <- get.val(dat, ind)
# datlist[["months_per_seas"]] <- get.vec(dat, ind)
# datlist[["Nsubseasons"]] <- get.val(dat, ind)
# datlist[["spawn_month"]] <- get.val(dat, ind)
# datlist[["Ngenders"]] <- get.val(dat, ind)
# datlist[["Nsexes"]] <- datlist[["Ngenders"]]
# datlist[["Nages"]] <- get.val(dat, ind)
# datlist[["N_areas"]] <- get.val(dat, ind)
# datlist[["Nfleets"]] <- get.val(dat, ind)
# datlist[["fleetinfo"]] <- get.df(dat, ind, datlist[["Nfleets"]])
# colnames(datlist[["fleetinfo"]]) <- c("type", 
#                                       "surveytiming", "area", "units", "need_catch_mult", 
#                                       "fleetname")
# if (any(datlist[["fleetinfo"]][["type"]] != 1 & 
#         datlist[["fleetinfo"]][["need_catch_mult"]] == 
#         1)) {
#   stop("Catch multipler can be used only for fleet_type = 1; Check fleet = ", 
#        paste0(which(datlist[["fleetinfo"]][["type"]] != 
#                       1 & datlist[["fleetinfo"]][["need_catch_mult"]] == 
#                       1), collapse = ", "), " in fleet info.")
# }
# if (echoall) {
#   message("Fleet information:")
#   print(datlist[["fleetinfo"]])
# }
# datlist[["fleetnames"]] <- datlist[["fleetinfo"]][["fleetname"]]
# datlist[["surveytiming"]] <- as.numeric(datlist[["fleetinfo"]][["surveytiming"]])
# datlist[["units_of_catch"]] <- as.numeric(datlist[["fleetinfo"]][["units"]])
# datlist[["areas"]] <- as.numeric(datlist[["fleetinfo"]][["area"]])
# if (any(datlist[["fleetinfo"]][["type"]] == 2)) {
#   nbycatch <- length(datlist[["fleetinfo"]][["type"]][datlist[["fleetinfo"]][["type"]] == 
#                                                         2])
#   datlist[["bycatch_fleet_info"]] <- get.df(dat, 
#                                             ind, nbycatch)
#   colnames(datlist[["bycatch_fleet_info"]]) <- c("fleetindex", 
#                                                  "includeinMSY", "Fmult", "F_or_first_year", 
#                                                  "F_or_last_year", "unused")
#   datlist[["bycatch_fleet_info"]] <- cbind(datlist[["bycatch_fleet_info"]], 
#                                            datlist[["fleetinfo"]][datlist[["fleetinfo"]][["type"]] == 
#                                                                     2, "fleetname", drop = FALSE])
# }
# datlist[["catch"]] <- get.df(dat, ind)
# colnames(datlist[["catch"]]) <- c("year", "seas", 
#                                   "fleet", "catch", "catch_se")
# datlist[["CPUEinfo"]] <- get.df(dat, ind, datlist[["Nfleets"]])
# CPUEinfo_names <- c("Fleet", "Units", "Errtype", 
#                     "SD_Report")
# colnames(datlist[["CPUEinfo"]]) <- CPUEinfo_names[1:ncol(datlist[["CPUEinfo"]])]
# rownames(datlist[["CPUEinfo"]]) <- datlist[["fleetnames"]]
# if (echoall) {
#   message("CPUE information:")
#   print(datlist[["CPUEinfo"]])
# }
# CPUE <- get.df(dat, ind)
# if (!is.null(CPUE)) {
#   datlist[["CPUE"]] <- CPUE
#   colnames(datlist[["CPUE"]]) <- c("year", 
#                                    "seas", "index", "obs", "se_log")
# } else {
#   datlist[["CPUE"]] <- NULL
# }
# if (echoall) {
#   message("CPUE data:")
#   print(datlist[["CPUE"]])
# }
# datlist[["N_discard_fleets"]] <- get.val(dat, ind)
# if (datlist[["N_discard_fleets"]]) {
#   datlist[["discard_fleet_info"]] <- get.df(dat, 
#                                             ind, datlist[["N_discard_fleets"]])
#   colnames(datlist[["discard_fleet_info"]]) <- c("Fleet", 
#                                                  "Units", "Errtype")
#   rownames(datlist[["discard_fleet_info"]]) <- datlist[["fleetnames"]][as.numeric(datlist[["discard_fleet_info"]][["Fleet"]])]
#   datlist[["discard_data"]] <- get.df(dat, ind)
#   colnames(datlist[["discard_data"]]) <- c("Yr", 
#                                            "Seas", "Flt", "Discard", "Std_in")
# } else {
#   datlist[["discard_fleet_info"]] <- NULL
#   datlist[["discard_data"]] <- NULL
# }
# if (echoall & !is.null(datlist[["discard_fleet_info"]])) {
#   message("Discard fleet information:")
#   print(datlist[["discard_fleet_info"]])
# }
# if (echoall & !is.null(datlist[["discard_data"]])) {
#   message("Discard data:")
#   print(datlist[["discard_data"]])
# }
# datlist[["use_meanbodywt"]] <- get.val(dat, ind)
# if (datlist[["use_meanbodywt"]]) {
#   datlist[["DF_for_meanbodywt"]] <- get.val(dat, 
#                                             ind)
#   datlist[["meanbodywt"]] <- get.df(dat, ind)
#   colnames(datlist[["meanbodywt"]]) <- c("Year", 
#                                          "Seas", "Fleet", "Partition", "Type", 
#                                          "Value", "Std_in")
# }else {
#   datlist[["DF_for_meanbodywt"]] <- NULL
#   datlist[["meanbodywt"]] <- NULL
# }
# if (verbose) {
#   message("use_meanbodywt (0/1): ", datlist[["use_meanbodywt"]])
# }
# if (echoall & !is.null(datlist[["meanbodywt"]])) {
#   message("meanbodywt:")
#   print(datlist[["meanbodywt"]])
# }
# datlist[["lbin_method"]] <- get.val(dat, ind)
# if (datlist[["lbin_method"]] == 2) {
#   bin_info_tmp <- get.val(dat, ind)
#   if (is.na(bin_info_tmp)) {
#     ind <- ind - 1
#     bin_info_tmp <- get.vec(dat, ind)
#     datlist[["binwidth"]] <- bin_info_tmp[1]
#     datlist[["minimum_size"]] <- bin_info_tmp[2]
#     datlist[["maximum_size"]] <- bin_info_tmp[3]
#   }
#   else {
#     datlist[["binwidth"]] <- bin_info_tmp
#     datlist[["minimum_size"]] <- get.val(dat, ind)
#     datlist[["maximum_size"]] <- get.val(dat, ind)
#   }
# } else if (datlist[["lbin_method"]] == 3) {
#   datlist[["N_lbinspop"]] <- get.val(dat, ind)
#   datlist[["lbin_vector_pop"]] <- get.vec(dat, ind)
# } else {
#   datlist[["binwidth"]] <- NULL
#   datlist[["minimum_size"]] <- NULL
#   datlist[["maximum_size"]] <- NULL
#   datlist[["N_lbinspop"]] <- NULL
#   datlist[["lbin_vector_pop"]] <- NULL
# }
# if (verbose) {
#   message("N_lbinspop: ", datlist[["N_lbinspop"]])
# }
# datlist[["use_lencomp"]] <- get.val(dat, ind)
# if (verbose) {
#   message("use_lencomp (0/1): ", datlist[["use_lencomp"]])
# }
# if (datlist[["use_lencomp"]]) { 
#   datlist[["len_info"]] <- get.df(dat, ind, datlist[["Nfleets"]])
#   # !!!: Warning message:
#   # In rbind("9", c("0", "1", "2", "3", "4", "5", "6", "7", "8"), "7",  :
#   #            number of columns of result is not a multiple of vector length (arg 2)
#   colnames(datlist[["len_info"]]) <- c("mintailcomp", 
#                                        "addtocomp", "combine_M_F", "CompressBins", 
#                                        "CompError", "ParmSelect", "minsamplesize")[1:ncol(datlist[["len_info"]])]
#   rownames(datlist[["len_info"]]) <- datlist[["fleetnames"]]
#   if (echoall) {
#     message("\nlen_info:")
#     print(datlist[["len_info"]])
#   }
#   datlist[["N_lbins"]] <- get.val(dat, ind)
#   # !!!: Warning message:
#   # In get.val(dat, ind) : NAs introduced by coercion
#   if (verbose) {
#     message("N_lbins: ", datlist[["N_lbins"]])
#   }
#   datlist[["lbin_vector"]] <- get.vec(dat, ind)
#   datlist[["lencomp"]] <- get.df(dat, ind)
#   # !!!: Warning message:
#   # In rbind(c("0.4032", "0.4032", "0.4995", "0.58", "0.6902", "0.8246",  :
#   #              number of columns of result is not a multiple of vector length (arg 1)
#   if (!is.null(datlist[["lencomp"]])) {
#     colnames(datlist[["lencomp"]]) <- c("Yr", 
#                                         "Seas", "FltSvy", "Gender", 
#                                         "Part", "Nsamp", if (abs(datlist[["Nsexes"]]) == 
#                                                              1) {
#                                           paste0("l", datlist[["lbin_vector"]])
#                                         } else {
#                                           NULL
#                                         }, if (datlist[["Nsexes"]] > 1) {
#                                           c(paste0("f", datlist[["lbin_vector"]]), 
#                                             paste0("m", datlist[["lbin_vector"]]))
#                                         } else {
#                                           NULL
#                                         })
#   }
#   if (!is.null(datlist[["lencomp"]])) {
#     zero_lencomp <- apply(datlist[["lencomp"]][, 
#                                                -(1:6)], MARGIN = 1, FUN = sum) == 0
#     if (any(zero_lencomp == TRUE)) {
#       warning("Lines of all zero length comp found. SS will exit on error if", 
#               " a line of comps is all zeros. Line(s) ", 
#               paste0(which(zero_lencomp), collapse = ", "))
#     }
#   }
#   # !!!: Warning message:
#   # Lines of all zero length comp found. SS will exit on error if a line of comps is all zeros. Line(s) 10, 11, 12, 13, 14, 15
#   if (echoall) {
#     message("\nFirst 2 rows of lencomp:")
#     print(head(datlist[["lencomp"]], 2))
#     message("\nLast 2 rows of lencomp:")
#     print(tail(datlist[["lencomp"]], 2))
#     cat("\n")
#   }
# }
# # !!!: Error in FUN(newX[, i], ...) : invalid 'type' (character) of argument
# # RW: running though the above line by line doesn't reproduce the error, 
# # and then cannot recreate after if run whole if-statement - must be creating something during troubleshooting
# 
# 
# datlist[["N_agebins"]] <- get.val(dat, ind)
# if (verbose) {
#   message("N_agebins: ", datlist[["N_agebins"]])
# }
# if (datlist[["N_agebins"]]) {
#   datlist[["agebin_vector"]] <- get.vec(dat, ind)
#   if (echoall) {
#     message("agebin_vector:")
#     print(datlist[["agebin_vector"]])
#   }
# } else {
#   datlist[["agebin_vector"]] <- NULL
# }
# if (datlist[["N_agebins"]]) {
#   datlist[["N_ageerror_definitions"]] <- get.val(dat, 
#                                                  ind)
#   if (datlist[["N_ageerror_definitions"]]) {
#     datlist[["ageerror"]] <- get.df(dat, ind, datlist[["N_ageerror_definitions"]] * 
#                                       2)
#     colnames(datlist[["ageerror"]]) <- paste0("age", 
#                                               0:datlist[["Nages"]])
#   }
#   else {
#     datlist[["ageerror"]] <- NULL
#   }
#   if (echoall) {
#     message("\nN_ageerror_definitions:")
#     print(datlist[["N_ageerror_definitions"]])
#     message("\nageerror:")
#     print(datlist[["ageerror"]])
#   }
# }
# if (datlist[["N_agebins"]]) {
#   datlist[["age_info"]] <- get.df(dat, ind, datlist[["Nfleets"]])
#   colnames(datlist[["age_info"]]) <- c("mintailcomp", 
#                                        "addtocomp", "combine_M_F", "CompressBins", 
#                                        "CompError", "ParmSelect", "minsamplesize")[1:ncol(datlist[["age_info"]])]
#   rownames(datlist[["age_info"]]) <- datlist[["fleetnames"]]
#   datlist[["Lbin_method"]] <- get.val(dat, ind)
#   if (echoall) {
#     message("\nage_info:")
#     print(datlist[["age_info"]])
#   }
# }
# if (datlist[["N_agebins"]]) {
#   datlist[["agecomp"]] <- get.df(dat, ind)
#   if (!is.null(datlist[["agecomp"]])) {
#     colnames(datlist[["agecomp"]]) <- c("Yr", 
#                                         "Seas", "FltSvy", "Gender", 
#                                         "Part", "Ageerr", "Lbin_lo", 
#                                         "Lbin_hi", "Nsamp", if (abs(datlist[["Nsexes"]]) == 
#                                                                 1) {
#                                           paste0("a", datlist[["agebin_vector"]])
#                                         } else {
#                                           NULL
#                                         }, if (datlist[["Nsexes"]] > 1) {
#                                           c(paste0("f", datlist[["agebin_vector"]]), 
#                                             paste0("m", datlist[["agebin_vector"]]))
#                                         } else {
#                                           NULL
#                                         })
#   }
#   if (!is.null(datlist[["agecomp"]])) {
#     zero_agecomp <- apply(datlist[["agecomp"]][, 
#                                                -(1:9)], MARGIN = 1, FUN = sum) == 0
#     if (any(zero_agecomp == TRUE)) {
#       warning("Lines of all zero age comp found. SS will exit on error if", 
#               " a line of comps is all zeros. Line(s) ", 
#               paste0(which(zero_agecomp), collapse = ", "))
#     }
#   }
#   if (echoall) {
#     message("\nFirst 2 rows of agecomp:")
#     print(head(datlist[["agecomp"]], 2))
#     message("\nLast 2 rows of agecomp:")
#     print(tail(datlist[["agecomp"]], 2))
#     cat("\n")
#   }
# } else {
#   if (verbose) {
#     message("N_agebins = 0, skipping read remaining age-related stuff")
#   }
# }
# if (any(datlist[["len_info"]][["CompError"]] == 
#         1) | any(datlist[["age_info"]][["CompError"]] == 
#                  1)) {
#   N_dirichlet_parms <- max(c(datlist[["len_info"]][["ParmSelect"]], 
#                              datlist[["age_info"]][["ParmSelect"]]))
#   N_dir_labs <- seq_len(N_dirichlet_parms)
#   for (i in N_dir_labs) {
#     if (!i %in% c(datlist[["len_info"]][["ParmSelect"]], 
#                   datlist[["age_info"]][["ParmSelect"]])) {
#       warning("Dirichlet multinomial parameters must be sequential with no ", 
#               " missing integers starting from 1. \nMissing DM parameter ", 
#               "labeled  ", i, ", so SS will exit on error for this model ", 
#               "configuration. \nPlease revise the numbering of the DM ", 
#               "parameters in the length/age info ParmSelect column.")
#     }
#   }
# }
# datlist[["use_MeanSize_at_Age_obs"]] <- get.val(dat, 
#                                                 ind)
# if (verbose) {
#   message("use_MeanSize_at_Age_obs (0/1): ", datlist[["use_MeanSize_at_Age_obs"]])
# }
# if (datlist[["use_MeanSize_at_Age_obs"]]) {
#   ind.tmp <- ind
#   endmwa <- ind - 2 + grep("-9999", dat[ind:length(dat)])[1]
#   xx <- dat[ind:endmwa]
#   if (length(unique(sapply(strsplit(xx, "\\s+"), 
#                            length))) > 1) {
#     if (verbose) {
#       message("Format of MeanSize_at_Age_obs appears to have sample sizes\n", 
#               "on separate lines than other inputs.")
#     }
#     xx <- paste(xx[1:length(xx)%%2 == 1], xx[1:length(xx)%%2 == 
#                                                0])
#   }
#   datlist[["MeanSize_at_Age_obs"]] <- data.frame(do.call("rbind", 
#                                                          strsplit(xx, "\\s+")), stringsAsFactors = FALSE)
#   ind <- endmwa + 1
#   test <- get.vec(dat, ind)
#   if (test[1] != -9999) {
#     warning("Problem with read of MeanSize_at_Age, terminator value != -9999")
#   }
#   colnames(datlist[["MeanSize_at_Age_obs"]]) <- c("Yr", 
#                                                   "Seas", "FltSvy", "Gender", "Part", 
#                                                   "AgeErr", "Ignore", if (abs(datlist[["Nsexes"]]) == 
#                                                                           1) {
#                                                     paste0("a", datlist[["agebin_vector"]])
#                                                   } else {
#                                                     NULL
#                                                   }, if (datlist[["Nsexes"]] > 1) {
#                                                     c(paste0("f", datlist[["agebin_vector"]]), 
#                                                       paste0("m", datlist[["agebin_vector"]]))
#                                                   } else {
#                                                     NULL
#                                                   }, if (abs(datlist[["Nsexes"]]) == 1) {
#                                                     paste0("N_a", datlist[["agebin_vector"]])
#                                                   } else {
#                                                     NULL
#                                                   }, if (datlist[["Nsexes"]] > 1) {
#                                                     c(paste0("N_f", datlist[["agebin_vector"]]), 
#                                                       paste0("N_m", datlist[["agebin_vector"]]))
#                                                   } else {
#                                                     NULL
#                                                   })
#   if (echoall) {
#     message("\nFirst 2 rows of MeanSize_at_Age_obs:")
#     print(head(datlist[["MeanSize_at_Age_obs"]], 
#                2))
#     message("\nLast 2 rows of MeanSize_at_Age_obs:")
#     print(tail(datlist[["MeanSize_at_Age_obs"]], 
#                2))
#     cat("\n")
#   }
#   test <- get.vec(dat, ind)
#   if (length(test) == 1) {
#     ind <- ind - 1
#   }
# } else {
#   datlist[["MeanSize_at_Age_obs"]] <- NULL
# }
# datlist[["N_environ_variables"]] <- get.val(dat, ind)
# if (verbose) {
#   message("N_environ_variables: ", datlist[["N_environ_variables"]])
# }
# if (datlist[["N_environ_variables"]]) {
#   datlist[["envdat"]] <- get.df(dat, ind)
#   colnames(datlist[["envdat"]]) <- c("Yr", 
#                                      "Variable", "Value")
#   if (echoall) {
#     message("\nFirst 2 rows of envdat:")
#     print(head(datlist[["envdat"]], 2))
#     message("\nLast 2 rows of envdat:")
#     print(tail(datlist[["envdat"]], 2))
#     cat("\n")
#   }
# } else {
#   datlist[["envdat"]] <- NULL
# }
# datlist[["N_sizefreq_methods"]] <- get.val(dat, ind)
# if (datlist[["N_sizefreq_methods"]]) {
#   datlist[["nbins_per_method"]] <- get.vec(dat, ind)
#   datlist[["units_per_method"]] <- get.vec(dat, ind)
#   datlist[["scale_per_method"]] <- get.vec(dat, ind)
#   datlist[["mincomp_per_method"]] <- get.vec(dat, 
#                                              ind)
#   datlist[["Nobs_per_method"]] <- get.vec(dat, ind)
#   if (echoall) {
#     message("Details of generalized size frequency methods:")
#     print(data.frame(method = 1:datlist[["N_sizefreq_methods"]], 
#                      nbins = datlist[["nbins_per_method"]], 
#                      units = datlist[["units_per_method"]], 
#                      scale = datlist[["scale_per_method"]], 
#                      mincomp = datlist[["mincomp_per_method"]], 
#                      nobs = datlist[["Nobs_per_method"]]))
#   }
#   datlist[["sizefreq_bins_list"]] <- list()
#   for (imethod in seq_len(datlist[["N_sizefreq_methods"]])) {
#     datlist[["sizefreq_bins_list"]][[imethod]] <- get.vec(dat, 
#                                                           ind)
#   }
#   datlist[["sizefreq_data_list"]] <- list()
#   for (imethod in seq_len(datlist[["N_sizefreq_methods"]])) {
#     Ncols <- 7 + abs(datlist[["Nsexes"]]) * datlist[["nbins_per_method"]][imethod]
#     Nrows <- datlist[["Nobs_per_method"]][imethod]
#     datlist[["sizefreq_data_list"]][[imethod]] <- get.df(dat, 
#                                                          ind, Nrows)
#     colnames(datlist[["sizefreq_data_list"]][[imethod]]) <- c("Method", 
#                                                               "Yr", "Seas", "FltSvy", "Gender", 
#                                                               "Part", "Nsamp", if (abs(datlist[["Nsexes"]]) == 
#                                                                                    1) {
#                                                                 paste0("a", datlist[["sizefreq_bins_list"]][[imethod]])
#                                                               } else {
#                                                                 NULL
#                                                               }, if (datlist[["Nsexes"]] > 1) {
#                                                                 c(paste0("f", datlist[["sizefreq_bins_list"]][[imethod]]), 
#                                                                   paste0("m", datlist[["sizefreq_bins_list"]][[imethod]]))
#                                                               } else {
#                                                                 NULL
#                                                               })
#     if (echoall) {
#       message("Method ", imethod, " (first two rows, ten columns):")
#       print(datlist[["sizefreq_data_list"]][[imethod]][1:min(Nrows, 
#                                                              2), 1:min(Ncols, 10)])
#     }
#     if (any(datlist[["sizefreq_data_list"]][[imethod]][, 
#                                                        "Method"] != imethod)) {
#       stop("Problem with method in size frequency data:\n", 
#            "Expecting method: ", imethod, "\n", 
#            "Read method(s): ", paste(unique(datlist[["sizefreq_data_list"]][["Method"]]), 
#                                      collapse = ", "))
#     }
#   }
# } else {
#   datlist[["nbins_per_method"]] <- NULL
#   datlist[["units_per_method"]] <- NULL
#   datlist[["scale_per_method"]] <- NULL
#   datlist[["mincomp_per_method"]] <- NULL
#   datlist[["Nobs_per_method"]] <- NULL
#   datlist[["sizefreq_bins_list"]] <- NULL
#   datlist[["sizefreq_data_list"]] <- NULL
# }
# datlist[["do_tags"]] <- get.val(dat, ind)
# if (datlist[["do_tags"]]) {
#   datlist[["N_tag_groups"]] <- get.val(dat, ind)
#   datlist[["N_recap_events"]] <- get.val(dat, ind)
#   datlist[["mixing_latency_period"]] <- get.val(dat, 
#                                                 ind)
#   datlist[["max_periods"]] <- get.val(dat, ind)
#   if (datlist[["N_tag_groups"]] > 0) {
#     Ncols <- 8
#     datlist[["tag_releases"]] <- get.df(dat, ind, 
#                                         datlist[["N_tag_groups"]])
#     colnames(datlist[["tag_releases"]]) <- c("TG", 
#                                              "Area", "Yr", "Season", "tfill", 
#                                              "Gender", "Age", "Nrelease")
#     if (echoall) {
#       message("Head of tag release data:")
#       print(head(datlist[["tag_releases"]]))
#     }
#   }
#   else {
#     datlist[["tag_releases"]] <- NULL
#   }
#   if (datlist[["N_recap_events"]] > 0) {
#     Ncols <- 5
#     datlist[["tag_recaps"]] <- get.df(dat, ind, 
#                                       datlist[["N_recap_events"]])
#     colnames(datlist[["tag_recaps"]]) <- c("TG", 
#                                            "Yr", "Season", "Fleet", "Nrecap")
#     if (echoall) {
#       message("Head of tag recapture data:")
#       print(head(datlist[["tag_recaps"]]))
#     }
#   }
#   else {
#     datlist[["tag_recaps"]] <- NULL
#   }
# }
# datlist[["morphcomp_data"]] <- get.val(dat, ind)
# if (datlist[["morphcomp_data"]]) {
#   warning("Morph comp data not yet supported by SS_readdat_3.30\n", 
#           "  Please post issue to https://github.com/r4ss/r4ss/issues\n", 
#           "  or email ian.taylor@noaa.gov", "if you want this functionality added.")
# }
# datlist[["use_selectivity_priors"]] <- get.val(dat, 
#                                                ind)
# eof <- get.val(dat, ind)
# if (verbose) {
#   if (Nsections == 1) {
#     message("Read of data file complete. Final value = ", 
#             eof)
#   }
#   else {
#     message("Read of section ", section, " of data file complete. Final value = ", 
#             eof)
#   }
# }
# datlist[["eof"]] <- FALSE
# if (eof == 999) 
#   datlist[["eof"]] <- TRUE
# datlist[["spawn_seas"]] <- datlist[["spawn_month"]]
# datlist[["Nfleet"]] <- nrow(subset(datlist[["fleetinfo"]], 
#                                    datlist[["fleetinfo"]][["type"]] <= 2))
# datlist[["Nsurveys"]] <- datlist[["Nfleets"]] - 
#   datlist[["Nfleet"]]
# totfleets <- datlist[["Nfleet"]] + datlist[["Nsurveys"]]
# if (nrow(datlist[["fleetinfo"]]) > 1) {
#   datlist[["fleetinfo1"]] <- t(datlist[["fleetinfo"]])
#   colnames(datlist[["fleetinfo1"]]) <- datlist[["fleetinfo"]][["fleetname"]]
#   datlist[["fleetinfo1"]] <- datlist[["fleetinfo1"]][1:5, 
#                                                      ]
#   datlist[["fleetinfo2"]] <- datlist[["fleetinfo1"]][4:5, 
#                                                      ]
#   datlist[["fleetinfo1"]] <- datlist[["fleetinfo1"]][c(2:3, 
#                                                        1), ]
#   rownames(datlist[["fleetinfo1"]]) <- c("surveytiming", 
#                                          "areas", "type")
#   datlist[["fleetinfo1"]] <- data.frame(datlist[["fleetinfo1"]])
#   datlist[["fleetinfo2"]] <- data.frame(datlist[["fleetinfo2"]])
# } else {
#   datlist[["fleetinfo1"]] <- NULL
#   datlist[["fleetinfo2"]] <- NULL
# }
# if (!is.null(datlist[["discard_fleet_info"]])) {
#   colnames(datlist[["discard_fleet_info"]]) <- c("Fleet", 
#                                                  "units", "errtype")
# }
# datlist[["catch"]] <- datlist[["catch"]][datlist[["catch"]][, 
#                                                             1] >= -999, ]
# colnames(datlist[["catch"]]) <- c("year", "seas", 
#                                   "fleet", "catch", "catch_se")
# if (datlist[["use_meanbodywt"]] == 0) {
#   datlist[["N_meanbodywt"]] <- 0
# }
# datlist[["comp_tail_compression"]] <- datlist[["len_info"]][["mintailcomp"]]
# datlist[["add_to_comp"]] <- datlist[["len_info"]][["addtocomp"]]
# datlist[["max_combined_lbin"]] <- datlist[["len_info"]][["combine_M_F"]]
# if (is.null(datlist[["lencomp"]])) 
#   datlist[["N_lencomp"]] <- 0
# if (datlist[["use_MeanSize_at_Age_obs"]] == 0) {
#   datlist[["N_MeanSize_at_Age_obs"]] <- 0
# }
# if (!is.null(datlist[["lbin_method"]])) {
#   if (datlist[["lbin_method"]] == 1) {
#     datlist[["N_lbinspop"]] <- datlist[["N_lbins"]]
#     datlist[["lbin_vector_pop"]] <- datlist[["lbin_vector"]]
#   }
#   if (datlist[["lbin_method"]] == 2) {
#     if (!is.null(datlist[["binwidth"]]) && !is.null(datlist[["minimum_size"]]) && 
#         !is.null(datlist[["maximum_size"]])) {
#       datlist[["N_lbinspop"]] <- (datlist[["maximum_size"]] - 
#                                     datlist[["minimum_size"]])/datlist[["binwidth"]] + 
#         1
#       datlist[["lbin_vector_pop"]] <- vector()
#       for (j in 0:datlist[["N_lbinspop"]]) {
#         datlist[["lbin_vector_pop"]] <- c(datlist[["lbin_vector_pop"]], 
#                                           datlist[["minimum_size"]] + (j * datlist[["binwidth"]]))
#       }
#     }
#   }
#   if (datlist[["lbin_method"]] == 3) {
#     if (!is.null(datlist[["lbin_vector_pop"]])) {
#       datlist[["N_lbinspop"]] <- length(datlist[["lbin_vector_pop"]])
#     }
#   }
# }
# return(datlist)

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

# # ---- troubleshooting -----
# traceback()
# 
# # code from create_sample_struct()
# dat = datfile
# nyrs = 6
# 
# assertive.types::assert_is_a_number(nyrs)
# if (length(dat) == 1 & is.character(dat)) {
#   dat <- SS_readdat(dat, verbose = FALSE)
# }
# list_name <- c("catch", "CPUE", "lencomp", "agecomp")
# sample_struct <- lapply(list_name,
#                         function(name, dat) {
#                           
# # try running apply individually
# 
# test1 <- function(name, dat) {  
#                           df <- dat[[name]]
#                           if (is.null(df)) {
#                             return(NA)
#                           }
#                           # get year, seas, fleet combo, ignoring -999 values.
#                           yr_col <- grep("year|yr", colnames(df), ignore.case = TRUE, value = TRUE)
#                           seas_col <- grep("seas", colnames(df), ignore.case = TRUE, value = TRUE)
#                           flt_col <- grep("FltSvy|fleet|index", colnames(df),
#                                           ignore.case = TRUE,
#                                           value = TRUE
#                           )
#                           input_SE_col <- grep("_se|se_", colnames(df),
#                                                ignore.case = TRUE,
#                                                value = TRUE
#                           ) # catch sample size
#                           Nsamp_col <- grep("Nsamp", colnames(df),
#                                             ignore.case = TRUE,
#                                             value = TRUE
#                           ) # input sample size
#                           # sanity checks. should match with 1 (or 0 in some cases) cols. Failing these
#                           # checks indicate a bug in the code (invalid assuptions of how to match the
#                           # cols.)
#                           assertive.properties::assert_is_of_length(yr_col, 1)
#                           assertive.properties::assert_is_of_length(seas_col, 1)
#                           assertive.properties::assert_is_of_length(flt_col, 1)
#                           # b/c only Nsamp or SE should exist for a df
#                           assertive.base::assert_is_identical_to_true(
#                             (length(input_SE_col) == 0 & length(Nsamp_col) == 1) |
#                               (length(input_SE_col) == 1 & length(Nsamp_col) == 0)
#                           )
#                           # find combinations of season and fleet in the df.
#                           df_combo <- unique(df[, c(seas_col, flt_col), drop = FALSE])
#                           fill_vec <- vector(mode = "list", length = nrow(df_combo))
#                           for (i in seq_len(nrow(df_combo))) {
#                             tmp_seas <- df_combo[i, seas_col]
#                             tmp_flt <- df_combo[i, flt_col]
#                             tmp_yrs <- df[df[[seas_col]] == tmp_seas &
#                                             df[[flt_col]] == tmp_flt &
#                                             df[[yr_col]] != -999, yr_col]
#                             tmp_yrs <- unique(tmp_yrs)
#                             tmp_yrs <- tmp_yrs[order(tmp_yrs)]
#                             
#                             
#                             # figure out diff between first and second yr.
#                             tmp_diff <- tmp_yrs[2] - tmp_yrs[1]
#                             # reconstruct the pattern
#                             pat <- seq(tmp_yrs[1], by = tmp_diff, length.out = length(tmp_yrs))
#                             if (all(!is.na(pat)) && all(pat == tmp_yrs)) { # a pattern was found
#                               future_pat <- seq(pat[length(pat)], dat[["endyr"]] + nyrs, by = tmp_diff)
#                               future_pat <- future_pat[future_pat > dat[["endyr"]]]
#                               if (length(future_pat) > 0) {
#                                 future_pat <- data.frame(
#                                   Yr = future_pat,
#                                   Seas = tmp_seas,
#                                   FltSvy = tmp_flt,
#                                   stringsAsFactors = FALSE
#                                 )
#                               } else {
#                                 message(
#                                   "Pattern found for ", name, ": FltSvy ", tmp_flt,
#                                   ", Seas ", tmp_seas, ", but no data to add for the ",
#                                   "timeframe specified. Returning NA for Yr in this ",
#                                   "dataframe."
#                                 )
#                                 future_pat <- data.frame(
#                                   Yr = NA,
#                                   Seas = tmp_seas,
#                                   FltSvy = tmp_flt,
#                                   stringsAsFactors = FALSE
#                                 )
#                               }
#                             } else {
#                               # the pattern was not found
#                               warning(
#                                 "Pattern not found for ", name, ": FltSvy ", tmp_flt,
#                                 ", Seas ", tmp_seas, ". Returning NA for Yr in this dataframe."
#                               )
#                               future_pat <- data.frame(
#                                 Yr = NA,
#                                 Seas = tmp_seas,
#                                 FltSvy = tmp_flt,
#                                 stringsAsFactors = FALSE
#                               )
#                             }
#                             if (name %in% c("lencomp", "agecomp")) {
#                               # Sex
#                               sex_col <- grep("Sex|Gender", colnames(df),
#                                               ignore.case = TRUE,
#                                               value = TRUE
#                               )
#                               tmp_sex <- unique(df[df[[seas_col]] == tmp_seas &
#                                                      df[[flt_col]] == tmp_flt, sex_col])
#                               if (length(tmp_sex) == 1) {
#                                 future_pat[["Sex"]] <- tmp_sex
#                               } else {
#                                 future_pat[["Sex"]] <- NA
#                               }
#                               # partition
#                               tmp_part <- unique(df[df[[seas_col]] == tmp_seas &
#                                                       df[[flt_col]] == tmp_flt, "Part"])
#                               if (length(tmp_part) == 1) {
#                                 future_pat[["Part"]] <- tmp_part
#                               } else {
#                                 future_pat[["Part"]] <- NA
#                               }
#                             }
#                             if (name == "agecomp") {
#                               # Ageerr, Lbin_lo, Lbin_hi
#                               tmp_err <- unique(df[df[[seas_col]] == tmp_seas &
#                                                      df[[flt_col]] == tmp_flt, "Ageerr"])
#                               if (length(tmp_sex) == 1) {
#                                 future_pat[["Ageerr"]] <- tmp_err
#                               } else {
#                                 future_pat[["Ageerr"]] <- NA
#                               }
#                               # Lbin_lo (expect should be -1)
#                               tmp_lo <- unique(df[df[[seas_col]] == tmp_seas &
#                                                     df[[flt_col]] == tmp_flt, "Lbin_lo"])
#                               if (length(tmp_lo) == 1) {
#                                 future_pat[["Lbin_lo"]] <- tmp_lo
#                               } else {
#                                 future_pat[["Lbin_lo"]] <- NA
#                               }
#                               # Lbin_hi (expect should be -1)
#                               tmp_hi <- unique(df[df[[seas_col]] == tmp_seas &
#                                                     df[[flt_col]] == tmp_flt, "Lbin_hi"])
#                               if (length(tmp_hi) == 1) {
#                                 future_pat[["Lbin_hi"]] <- tmp_hi
#                               } else {
#                                 future_pat[["Lbin_hi"]] <- NA
#                               }
#                             }
#                             # add sample size, if possible
#                             # see if se or Nsamp is the same across years for the seas/flt. If so,
#                             # add to the df. If not, add NA's.
#                             if (length(input_SE_col) == 1) {
#                               tmp_SE <- unique(df[df[[seas_col]] == tmp_seas &
#                                                     df[[flt_col]] == tmp_flt &
#                                                     df[[yr_col]] != -999, input_SE_col])
#                               if (length(tmp_SE) == 1) {
#                                 future_pat[["SE"]] <- tmp_SE
#                               } else {
#                                 future_pat[["SE"]] <- NA
#                                 warning("NA included in column SE for ", name, ".")
#                               }
#                             }
#                             if (length(Nsamp_col) == 1) {
#                               tmp_Nsamp <- unique(df[df[[seas_col]] == tmp_seas &
#                                                        df[[flt_col]] == tmp_flt &
#                                                        df[[yr_col]] != -999, Nsamp_col])
#                               if (length(tmp_Nsamp) == 1) {
#                                 future_pat[["Nsamp"]] <- tmp_Nsamp
#                               } else {
#                                 future_pat[["Nsamp"]] <- NA
#                                 warning("NA included in column Nsamp for ", name, ".")
#                               }
#                             }
#                             fill_vec[[i]] <- future_pat
#                           }
#                           future_pat_all <- do.call("rbind", fill_vec)
#                         }#,
# #                         dat = dat
# # )
# name <- list_name[4]
# sample_struct <- test1(name, dat)
# # !!!RW: something in the definition of age-length comps making the error:
# #Error in `[[<-.data.frame`(`*tmp*`, "Ageerr", value = 1:4) : 
# #  replacement has 4 rows, data has 1
# names(sample_struct) <- list_name
# sample_struct


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

sample_struct_list <- list("constGrowth" = sample_struct)
# Run the OM --------------------------------------------------------------

run_res_path <- file.path("C:/Users/Robert W/Documents/FutureSeas/SardineMSE", "results")
# dir.create(run_res_path)
run_SSMSE(scen_name_vec = "constGrowth",# name of the scenario
          out_dir_scen_vec = mseOutputPath, # directory in which to run the scenario
          iter_vec = c(2), # run with 2 iterations for now
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = OMmodelPath, # OM files
          EM_name_vec = "constGrowthSelfTest", # cod is included in package data
          EM_in_dir_vec = OMmodelPath, # EM files
          MS_vec = "EM",       # The management strategy is specified in the EM
          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
          nyrs_vec = 2,        # Years to project OM forward
          nyrs_assess_vec = 1, # Years between assessments
          # rec_dev_pattern = "rand", # Use random recruitment devs
          # scope = "2", # to use the same recruitment devs across scenarios.
          # impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = FALSE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 12345) #Set a fixed integer seed that allows replication


# Summarize results -------------------------------------------------------

# Summarize 1 iteration of output
sumry <- SSMSE_summary_all(run_res_path)

sumry$ts[, c("model_run", "year", "SpawnBio", "iteration")]
# Get rid of duplicated SSB years
tsMod <- na.omit(sumry$ts)

ggplot2::ggplot(data = subset(tsMod, model_run %in% c("constGrowth_test2_OM", 
                                                         "constGrowthSelfTest_EM_2020")), 
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
