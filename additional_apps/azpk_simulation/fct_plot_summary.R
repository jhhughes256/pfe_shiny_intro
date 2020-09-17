#
#' Create list object of plot summary statistics
#' 
#' @description 
#' Generates a list of functions to be fed to a summarise_at function to provide
#' summary statistics for plotting. List consists of the median and the 
#' requested prediction intervals (percentiles).
#' 
#' @param civals Desired prediction intervals for summary statistic list
#' 
#' @return List of functions that can be used with summarise_at
#' @keywords internal
#' @export
#' 
fct_plot_summary <- function(pivals = c(0.9, 0.8, 0.6, 0.4, 0.2)) {
  if (any(is.na(pivals))) {
    list(~median(., na.rm = TRUE))
  } else {
    purrr::map(pivals, function(pival) {
        pilo <- (1-pival)/2 # Lower percentile
        pihi <- pival+pilo # Upper percentile
        out <- list(
          as.formula(paste0("~quantile(., probs = ", pilo, ", na.rm = TRUE)")),
          as.formula(paste0("~quantile(., probs = ", pihi, ", na.rm = TRUE)"))
      )}) %>%
      unlist(recursive = FALSE) %>% 
      {purrr::list_merge(list(~median(., na.rm = TRUE)), .)} %>% 
      unlist(recursive = FALSE) %>% 
      magrittr::set_names(c("median", paste0(
        "pi", rep(pivals*100, each = 2), rep(c("lo", "hi"), times = length(pivals))
      )))  # paste0, c, set_names
  }
}