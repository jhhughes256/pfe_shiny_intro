#
#' Generate application table
#' 
#' @description 
#' Generates table for either current or saved mrgsolve simulation data. 
#' Table contains number of individuals simulated, effective concentrations and
#' time spent above effective concentrations in AM and lung tissue. Displays 
#' single value when simulating one individual, and displays median and 90% 
#' prediction intervals when simulating more than one individual. Rounds values
#' to one decimal place. 
#' 
#' @param rsim mrgsolve model simulation data.frame
#' @param label Text label to give the first column of the table
#' 
#' @return Table for display in application. If input is NULL produces empty
#' table.
#' @keywords internal
#' @export
#' 
fct_process_table <- function(rsim, label) {
  nid <- length(unique(rsim$ID))
  metric <- ifelse(nid > 1, "Median (90% PI)", "Value")
  rsim %>%
    purrr::when(
      !is.null(.) ~ rsim %>%
        dplyr::filter(time == max(time)) %>%
        dplyr::select(tidyselect::contains("EC"), tidyselect::contains("WT")) %>%
        dplyr::mutate_at(dplyr::vars(tidyselect::contains("TEC")), 
          magrittr::divide_by, 24) %>%
        dplyr::group_by(AZEC50, AZEC90, WT) %>%
        dplyr::summarise_all(fct_plot_summary(ifelse(nid > 1, 0.9, NA))) %>%
        purrr::when(
          nid != 1 ~ tidyr::pivot_longer(., tidyselect::contains("TEC")) %>%
            tidyr::separate("name", c("metric", "stat"), sep = "_") %>%
            dplyr::mutate(value = round(value, 1)) %>%
            tidyr::pivot_wider(names_from = "stat", values_from = "value") %>%
            dplyr::mutate(value = paste0(median, " (", pi90lo, " - ", pi90hi, ")")) %>%
            dplyr::select(-median, -pi90lo, -pi90hi) %>%
            tidyr::pivot_wider(names_from = "metric", values_from = "value"),
          nid == 1 ~ dplyr::mutate_at(., dplyr::vars(-dplyr::group_cols()), round, 1)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(NID = nid) %>%
        dplyr::select(
          "Number of Individuals" = NID,
          "Population Body Weight (kg)" = WT,
          "EC50 (ng/mL)" = AZEC50,
          "Time above EC50 in AM (days)" = ALMATEC50,
          "Time above EC50 in lung (days)" = LUNGTEC50,
          "EC90 (ng/mL)" = AZEC90,
          "Time above EC90 in AM (days)" = ALMATEC90,
          "Time above EC90 in lung (days)" = LUNGTEC90) %>%
        dplyr::mutate_all(as.character),
      is.null(.) ~ tibble::tibble(
        "Number of Individuals" = "-",
        "Population Body Weight (kg)" = "-",
        "EC50 (ng/mL)" = "-",
        "Time above EC50 in AM (days)" = "-",
        "Time above EC50 in lung (days)" = "-",
        "EC90 (ng/mL)" = "-",
        "Time above EC90 in AM (days)" = "-",
        "Time above EC90 in lung (days)" = "-")) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(), 
      names_to = label, values_to = metric)
}