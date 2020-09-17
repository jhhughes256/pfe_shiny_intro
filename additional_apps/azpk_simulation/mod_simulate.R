# Module UI
  
#' @title   mod_simulate_ui and mod_simulate_server
#' @description  Shiny module which handles all dosing regimen input. Includes 
#' buttons to start simulation and save/clear simulation output for comparison.
#' Child module of mod_regimen.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_simulate
#'
#' @keywords internal
#' @export 
#' @import shiny

mod_simulate_ui <- function(id) {
# Define namespace function for IDs
  ns <- NS(id)
# Create tagList to be used in the UI
# * dashboardButton wrapper function can be found in `utils.R`
  div(align = "center",
    fct_dashboardButton(ns("sim"), "Update Dosing Regimen",
      status = "warning", width = "100%"),  # dashboardButton
    br(), br(),  # spacing
    fluidRow(
      column(6,
        fct_dashboardButton(ns("save"), "Save Current Output",
          status = "warning", width = "100%")  # dashboardButton
      ),
      column(6,
        fct_dashboardButton(ns("clear"), "Clear Saved Output",
          status = "warning", width = "100%")  # dashboardButton
      )
    ),  # div
    hr(),
    h4(strong("For Research Use Only"))
  )  # tagList
}  # mod_simulate_ui

# Module Server
    
#' @rdname mod_simulate
#' @export
#' @keywords internal

mod_simulate_server <- function(input, output, session, Rinput, rv) {
# Create reactiveValues object to store simulation output
# * `rsim$out` contains the current output from mrgsolve simulation
# * `rsim$save` contains the saved output from mrgsolve simulation
  rsim <- reactiveValues(
    out = NULL,
    save = NULL,
    out_reg = NULL,
    save_reg = NULL
  )
  
# Observe simulate button, when pressed:
# * check to make sure that Rinput() has been called successfully, if it was...
# * check to make sure that the dosing interval isn't greater than the dosing
#   duration
#     + if it is, set the duration to the dosing interval and provide a warning
# * check to make sure that the dosing interval divides evenly into the dosing
#   duration
#     + if it doesn't, remove the remainder from the interval to obtain an 
#       appropriate value and provide a warning
#     + then run the simulation with an adjusted Rinput() list as it won't 
#       be invalidated until the end of the observeEvent
# * run fct_simulate_model with the input from Rinput() and store in current 
#   output `rsim$out`
# * if Rinput() wasn't called successfully, let user know that they clicked the
#   simulation button a bit too quickly and should try again.
# * ignoreNULL = FALSE ensures that it runs on Shiny app initiation
  observeEvent(input$sim, {
    if (!"try-error" %in% class(Rinput())) {
    # Save current Rinput values to rv
      names <- names(Rinput())
      index <- purrr::map(names, function(name) {
        if (name %in% c("amt", "int", "dur")) { seq(1, rv$n, by = 1) }
        else { 1 } })
      purrr::walk2(names, index, ~ { rv[[.x]][.y] <- na.omit(Rinput()[[.x]][.y]) })
    }
  # Check that dosing interval isn't greater than dosing duration
    reg_check1 <- rv$int > (24*rv$dur)
    if (any(reg_check1)) {
      rv$dur[which(reg_check1)] <- rv$int[which(reg_check1)]/24
      showNotification(paste("The dosing interval must be shorter than the",  
        "dosing duration. Dosing duration was extended prior to simulation."), 
        type = "error", duration = 30)
    } 
  # Check that dosing interval divides evenly into dosing duration
    reg_check2 <- (24*rv$dur) %% rv$int
    if (any(reg_check2 != 0)) {
      rv$int[which(reg_check2 != 0)] <- rv$int[which(reg_check2 != 0)] %>%
        magrittr::subtract(reg_check2[reg_check2 != 0])
      showNotification(paste("The dosing interval must be a multiple of the",  
        "dosing duration. Dosing interval was reduced prior to simulation."), 
        type = "error", duration = 30)
    }
  # Run simulation
    rsim$out <- fct_simulate_model(rv, session)
    rsim$out_reg <- c("amt", "int", "dur") %>%
      purrr::map(~ rv[[.x]][seq(1, rv$n, by = 1)]) %>%
      magrittr::set_names(c("amt", "int", "dur"))
  }, ignoreNULL = FALSE)  # observeEvent
  
# Observe save button, when pressed save current output to `rsim$save`
  observeEvent(input$save, {
    rsim$save <- rsim$out
    rsim$save_reg <- rsim$out_reg
  })  # observeEvent
  
# Observe clear button, when pressed clear current output from `rsim$save`
  observeEvent(input$clear, {
    rsim$save <- NULL
    rsim$save_reg <- NULL
  })  # observeEvent
  
# Return rout object to parent module (mod_regimen_server)
  return(rsim)

}  # mod_simulate_server
