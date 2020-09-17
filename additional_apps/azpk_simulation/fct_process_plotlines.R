#
#' Generate data for plotting lines from simulation data
#' 
#' @description 
#' Generates data.frame suitable for plotting lines with ggplot2 from mrgsolve
#' simulation output. 
#' 
#' @param rsim mrgsolve model simulation data.frame
#' @param plot_summary_arg arguments to provide fct_plot_summary
#' 
#' @return data.frame suitable for plotting lines
#' @keywords internal
#' @export
#' 
# Create function for generating plotted lines from simulation data
# * function is applied to both current and saved outputs
# * removes TEC50 and TEC90 columns prior to processing
fct_process_plotlines <- function(rsim, plot_summary_arg) {
  rsim %>%
    dplyr::select(-tidyselect::contains("EC"), -tidyselect::contains("WT")) %>%
    dplyr::mutate(time = time/24) %>%
    tidyr::pivot_longer(cols = -c("ID", "time"),
      names_to = "Tissue", values_to = "DV") %>%
    dplyr::mutate(Tissue = factor(Tissue, 
      labels = c("AM", "Lung", "Plasma", "WBC"))) %>%
    dplyr::group_by(time, Tissue) %>%
    purrr::when(
      length(unique(.$ID)) == 1 ~ dplyr::summarise(., median = median(DV)),
      length(unique(.$ID)) > 1 ~ dplyr::summarise_at(., "DV", 
        fct_plot_summary(plot_summary_arg))
    ) %>%
    dplyr::ungroup()
}