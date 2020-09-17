#' Simulate Azithromycin Population Pharmacokinetic Model
#' 
#' @description 
#' Performs simulations of the Azithromycin Population Pharmacokinetic Model
#' based on the patient characteristics and treatment regimen input by the
#' user.
#' 
#' @param input List of inputs to be used during simulation
#' @param session internal
#' 
#' @return A dataframe containing time and dependent variable values in multiple
#' tissues.
#' @keywords internal
#' @export
#' 

fct_simulate_model <- function(input, session) {
# Define dose regimen based on input values `amt`, `int` and `dur`
  dose_ev <- purrr::map_dfr(seq_along(1:input$n), function(i) {
    tibble::tibble(
      amt = rep(input$amt[[i]], 24*input$dur[[i]]/input$int[[i]]),
      tafo = 24*seq(0, input$dur[[i]] - input$int[[i]]/24, by = input$int[[i]]/24),
      time = tafo + ifelse(i == 1, 0, 24*sum(input$dur[seq(1, (i - 1), by = 1)]))
    )
  })
  
# Extract compiled model from session$userData
  mod <- session$userData[["mod"]]
  
# Extract omega matrix from mrgsolve model
  mrgomega <- mrgsolve::omat(mod, make = TRUE)
  
# Randomly generate ETA values for each individual from a multi-variate normal
# distribution.
  if (input$nid > 1) {
    mrgidata <- mrgomega %>%
      {MASS::mvrnorm(n = input$nid, mu = rep(0, times = nrow(.)), Sigma = .)} %>%
      tibble::as_tibble() %>%
      magrittr::set_names(paste0("ETA", 1:nrow(mrgomega))) %>%
      tibble::add_column(ID = 1:(input$nid), .before = "ETA1")
  }
  
# Simulate model with the following settings:
# * Body weight set to value from input
# * Dosing records set to values from input
# * If more than one subject was simulated, include random ETA values
# * Simulate model
  mrgout <- mod %>%
    mrgsolve::param(WT = input$bwt, AZEC50 = input$ec50, AZEC90 = input$ec90) %>%
    mrgsolve::ev(amt = dose_ev$amt, time = dose_ev$time) %>%
    purrr::when(
      input$nid == 1 ~ .,
      input$nid > 1 ~ mrgsolve::idata_set(., mrgidata)) %>%
    mrgsolve::mrgsim_df(end = 24*input$endtime, delta = 24*input$endtime/300, 
      Request = c("WT", "CPLAST", "CLUNG", "CWBCT", "CALMAT", 
        "AZEC50", "AZEC90", "ALMATEC50", "LUNGTEC50", "ALMATEC90", "LUNGTEC90")
    )  # mrgsim_df
  
# Return model simulations as output
  return(mrgout)
  
}  # fct_simulate_model