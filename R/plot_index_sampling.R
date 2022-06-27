# RW: New plot_index_sampling() to handle different naming on scenarios

#' Plot index data, expected values, and sampled data for 1 scenario
#'
#' Creates a plot that can be used to see how sampling lines up with 
#' data and expected values for the index of abundance
#' @param dir Path to the directory containing 1 scenario. Defaults to
#'  the current working directory.
#' @export
#' @author Kathryn Doering
#' @return A list containing 2 components: 1) the ggplot object and 2) the
#'  dataframe used to make the ggplot object
plot_index_sampling <- function(dir = getwd()) {
  # get the iterations
  iters <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  scenario <- basename(dir)
  # get the OM data values, which are the same across iterations.
  om_name <- grep("_OM", list.dirs(file.path(dir, iters[1]),
                                  recursive = FALSE,
                                  full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(om_name)
  tmp_dat_OM <- r4ss::SS_readdat(file.path(
    dir, as.character(iters[1]),
    om_name, "data.ss_new"
  ),
  verbose = FALSE, section = 1
  )
  tmp_dat_OM[["CPUE"]][["iteration"]] <- as.character(iters[1])
  tmp_dat_OM[["CPUE"]][["scenario"]] <- scenario
  tmp_dat_OM[["CPUE"]][["model_run"]] <- "historical_values"
  index_dat <- tmp_dat_OM[["CPUE"]]
  # get the OM expected values
  tmp_dat_OM <- r4ss::SS_readdat(file.path(
    dir, as.character(iters[1]),
    om_name, "data.ss_new"
  ),
  verbose = FALSE, section = 2
  )
  tmp_dat_OM[["CPUE"]][["iteration"]] <- as.character(iters[1])
  tmp_dat_OM[["CPUE"]][["scenario"]] <- scenario
  tmp_dat_OM[["CPUE"]][["model_run"]] <- "OM_expected_values"
  index_dat <- rbind(index_dat, tmp_dat_OM[["CPUE"]])
  # get the EM init values
  # em name is the same across iterations
  em_name <- grep("EM_init$", list.dirs(file.path(dir, iters[1]),
                                        recursive = FALSE,
                                        full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(em_name)
  for (i in iters) {
    tmp_dat_EM <- r4ss::SS_readdat(file.path(
      dir, as.character(i),
      em_name, "data.ss_new"
    ),
    verbose = FALSE, section = 1
    )
    tmp_dat_EM[["CPUE"]][["iteration"]] <- i
    tmp_dat_EM[["CPUE"]][["scenario"]] <- scenario
    tmp_dat_EM[["CPUE"]][["model_run"]] <- "sampled_dataset"
    index_dat <- rbind(index_dat, tmp_dat_EM[["CPUE"]])
  }
  # get rid of negative index values
  index_dat <- index_dat[index_dat[["index"]] > 0, ]
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    # this is unlikely to be used, as  parallel is installed with base R.
    warning(
      "The package parallel is not available.",
      " Returning early with just the dataframe and not the plot."
    )
    return(list(index_dat = index_dat, index_plot = NA))
  }
  index_plot <- ggplot2::ggplot(
    index_dat,
    ggplot2::aes(x = .data[["year"]], y = .data[["obs"]])
  ) +
    ggplot2::geom_line(ggplot2::aes(
      linetype = .data[["iteration"]],
      color = .data[["model_run"]]
    )) +
    ggplot2::scale_linetype_manual(
      values = rep("solid", length(unique(index_dat[["iteration"]])))
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["index"]])) + # by fleet
    ggplot2::guides(linetype = FALSE) +
    ggplot2::theme_classic()
  
  index_list <- list(index_dat = index_dat, index_plot = index_plot)
}