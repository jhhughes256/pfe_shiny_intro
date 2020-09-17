#
#' Generate regimen text
#' 
#' @description 
#' Generates regimen text to appear above the table based on dosing regimen
#' input. Adds additional breaks below the regimen text for current output based 
#' on the number of rows of the regimen text for the other output and vice versa. 
#' 
#' @param reg Regimen data from output in list format
#' @param other Regimen data from other output in list format
#' 
#' @return Text describing the regimen with appropriate spacing
#' @keywords internal
#' @export
#' 
fct_process_regimentext <- function(reg, other) {
  if (!is.null(reg)) {
    seq_along(reg$amt) %>%
      purrr::map(function(i) {
        dur_txt <- paste0("Day", ifelse(reg$dur[[i]] > 1, "s ", " "))
        dur_val <- reg$dur %>% 
          purrr::when(
            .[[i]] == 1 & i == 1 ~ "1",
            .[[i]] != 1 & i == 1 ~ paste("1", .[[i]], sep = "-"),
            .[[i]] == 1 & i != 1 ~ paste(.[[i-1]] + 1),
            .[[i]] != 1 & i != 1 ~ paste(.[[i-1]] + 1, .[[i-1]] + .[[i]], sep = "-")
          )  # when
        dur_str <- paste0(dur_txt, dur_val, ":")
        amt_str <- paste(reg$amt[[i]], "mg")
        int_str <- paste("every", reg$int[[i]], "hours")
        paste(dur_str, amt_str, int_str)
      }) %>%  # map
      purrr::map(strong) %>%
      purrr::map(h5) %>%
      purrr::when(
        is.null(other) ~ .,
        length(.) >= length(other$amt) ~ .,
        length(.) < length(other$amt) ~ list(.,
          purrr::map(
            seq(length(reg$amt) + 1, length(other$amt), by = 1), ~ h5(br()))) %>%
          unlist(recursive = FALSE)
      ) %>%  # when
      return()
  } else if (!is.null(other)) {
    seq_along(other$amt) %>%
      purrr::map(~ h5(br())) %>%
      return()
  } else {
    return("")
  }
}