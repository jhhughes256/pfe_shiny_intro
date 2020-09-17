#
#' Generate plotted ribbon from summary data
#' 
#' @description 
#' Generates data.frame suitable to plot a ribbon in ggplot2 using summary data
#' from fct_process_plotlines.
#' 
#' @param lines Summary data from fct_process_plotlines
#' 
#' @return data.frame suitable for plotting ribbons
#' @keywords internal
#' @export
#' 
# Create function for extracting prediction intervals from summary data
  fct_process_plotribbons <- function(lines) {
    lines %>%
      dplyr::select(time, Tissue, tidyselect::contains("pi")) %>%
      tidyr::pivot_longer(cols = tidyselect::contains("pi"), 
        names_to = "PI", values_to = "value") %>%
      tidyr::separate(col = "PI", into = c("PI", "id"), sep = -2) %>%
      tidyr::pivot_wider(id_cols = c("time", "Tissue", "PI"),
        names_from = "id", values_from = "value") %>%
      dplyr::mutate(PI = paste(Tissue, PI))
  }
  
