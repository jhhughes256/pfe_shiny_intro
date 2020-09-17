# Module UI
  
#' @title   mod_regimen_ui and mod_regimen_server
#' @description  Shiny module which handles all dosing regimen input. Includes
#' dynamic dose regimen entry field with up to three rows to enter customised
#' dosing regimens. Also allows to choose from presets which are based on 
#' current dosing regimens used for NCT COVID-19 trials. Parent module of 
#' mod_simulate.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_regimen
#'
#' @keywords internal
#' @export 
#' @import shiny

mod_regimen_ui <- function(id) {
# Define namespace function for IDs
  ns <- NS(id)
# Create tagList to be used in the UI
# * box with width according to parent div, footer from child mod_simulate
# * dynamic input box UI from server called using uiOutput
# * action buttons "add" and "remove" are observed, interact with dynamic UI
# * selectInput choices are observed, interact with dynamic UI
  box(width = "100%", title = "Regimen Information", align = "center",
  # Customise style tags for slider input
    tags$style(HTML(".irs-grid-text, .irs-min, .irs-max {color:#000000;
      font-size:12px}")),
    tags$style(HTML(".irs-single, .irs-from, .irs-to {font-size:12px}")),
    tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar {background: #F39C12}")),
    tags$style(HTML(".irs-bar {border-top: #F39C12; border-bottom: #F39C12}")),
    tags$style(HTML(".irs-bar-edge {border: #F39C12}")),
  # Begin UI content
    h4(strong("Patient Population")), br(),
    shinyWidgets::sliderTextInput(ns("nid"), 
      label = "Population Size for Simulation:",
      choices = c(20, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), 
      grid = TRUE, width = "95%"),
    fluidRow(
      column(6,
        numericInput(ns("bwt"), "Mean Body Weight (kg):",
          value = 79, min = 1, step = 0.1)
      ),  # column
      column(6,
        numericInput(ns("endtime"), "Simulation Duration (days):",
          value = 21, min = 7, max = 84, step = 1)
      )  # column
    ),  # fluidRow
    hr(), h4(strong("Azithromycin Treatment")), br(),
    selectInput(ns("choose"), "Select dosing regimen:",
      choices = c(
        "Days 1-3: 500 mg" = 1,
        "Day 1: 500 mg; Days 2-5: 250 mg" = 2,
        "Day 1: 1000 mg" = 3,
        "Days 1-10: 500 mg" = 4,
        "User-Defined" = 0
      ), selected = 1),
    conditionalPanel("input.choose == '0'", ns = ns,
      uiOutput(ns("regimen")),
      br(),
      div(style = "display:in-line",
        actionButton(ns("add"), "Add"),
        actionButton(ns("rem"), "Remove")
      )  # div
    ),  # conditionalPanel
    hr(), h4(strong("In Vitro EC50 and EC90")), br(),
    selectInput(ns("ecref"), "Select reference:",
      choices = c(
        "SARS-CoV-2 (Touret F et al. 2020)" = 1,
        "User-Defined" = 0
      ), selected = 1),
    conditionalPanel("input.ecref == '0'", ns = ns,
      column(6,
        numericInput(ns("ec50"), "EC50 (ng/mL):", value = 1600)
      ),  # column
      column(6,
        numericInput(ns("ec90"), "EC90 (ng/mL):", value = 6500)
      )  # column
    ),  # conditionalPanel
    footer = mod_simulate_ui(ns("sim")), 
    status = "warning", solidHeader = TRUE
  )  # box
}  # mod_regimen_ui

# Module Server
    
#' @rdname mod_regimen
#' @export
#' @keywords internal

mod_regimen_server <- function(input, output, session) {
# Define namespace function for IDs
  ns <- session$ns
  
# Create reactiveValues object to store renderUI input values
# * n is the number of dosing regimens 
#     + it dictates the number of rendered input boxes as well as how many
#       amt, int and dur values are used when passing to the model using `ev`
# * amt, int and dur are vectors for the dose, interval and duration for the
#   first, second and third dosing regimen, pasted to `ev`
#     + if no second or third dosing regimen exists, values in those positions
#       are ignored
  rv <- reactiveValues(
    n = 1,  # number of rendered input boxes (min: 1, max: 3)
    amt = rep(500, 3),
    int = rep(24, 3),
    dur = rep(3, 3),
    bwt = 79,
    ec50 = 1600,
    ec90 = 6500,
    nid = 1,
    choose = "1",
    endtime = 21
  )
  
# Define reactive function for dynamic input box ui
# * map used for creating the same set of input boxes `rv$n` times
# * ilab and padding are defined so that only one label is rendered for all
#   `rv$n` input boxes
# * fluidRow wrapped in column to remove padding issues
# * div used to define column function with width that applies to any size screen
# * inputId given standardised name depending on the row of input boxes it is in
# * value equals corresponding `rv` value indexed using input box row
  Rui <- reactive({
    purrr::map(seq(1, rv$n, by = 1), function(i) {
      if (i == 1) { 
        ilab <- c("Dose:", "Interval:", "Duration:") 
        padding <- "padding-top:20px; padding-left:0px"
      } else { 
        ilab <- NULL 
        padding <- "padding-left:0px"
      }
      column(12, fluidRow(
        div(class = "col-xs-3",
          numericInput(ns(paste0("amt", i)), 
            label = ilab[[1]], 
            value = rv$amt[[i]])
        ), # div
        div(class = "col-xs-1",
          style = padding,
          h5(strong("mg, every"))
        ), # column
        div(class = "col-xs-3",
          numericInput(ns(paste0("int", i)), 
            label = ilab[[2]], 
            value = rv$int[[i]])
        ), # column
        div(class = "col-xs-1",
          style = padding,
          h5(strong("hours, for"))
        ), # column
        div(class = "col-xs-3",
          numericInput(ns(paste0("dur", i)), 
            label = ilab[[3]], 
            value = rv$dur[[i]])
        ), # column
        div(class = "col-xs-1",
          style = padding,
          h5(strong("days"))
        )  # column
      ))  # column.fluidRow
    })  # map
  })  # reactive
  
# Define reactive function containing EC50 & EC90 values based on input
# * Define literature values in vitro effective concentrations `ecref_lst`
# * If reference EC value is selected, use `input$ecref` as index for `ecref_lst`
# * Else if user-defined value is provided 
  Recref <- reactive({
    ecref_lst <- list(
      "1" = list(ec50 = 1600, ec90 = 6500)  # Touret F et al. 2020
    )  # ecref_lst
    if (input$ecref != "0") {
      return(ecref_lst[[input$ecref]])
    } else if (input$ecref == "0") {
      return(list(ec50 = input$ec50, ec90 = input$ec90))
    }
  })
  
# Define reactive function containing input values
# * try helps protect Rinput being called before dynamic input is generated
# * when allows for the use of Rinput() for simulation, which helps when having
#   user-defined regimens
# * map_dbl collects each input box value using standardised naming scheme
  Rinput <- reactive({
    input$choose %>%
      purrr::when(
        . == "0" ~ try(list(
          amt = purrr::map_dbl(seq(1, rv$n, by = 1), ~ input[[paste0("amt", .x)]]),
          int = purrr::map_dbl(seq(1, rv$n, by = 1), ~ input[[paste0("int", .x)]]),
          dur = purrr::map_dbl(seq(1, rv$n, by = 1), ~ input[[paste0("dur", .x)]]))),
        . != "0" ~ try(list(
          amt = purrr::map_dbl(seq(1, rv$n, by = 1), ~ rv$amt[[.x]]),
          int = purrr::map_dbl(seq(1, rv$n, by = 1), ~ rv$int[[.x]]),
          dur = purrr::map_dbl(seq(1, rv$n, by = 1), ~ rv$dur[[.x]])))) %>%
      purrr::list_modify(
        bwt = input$bwt,
        ec50 = Recref()$ec50,
        ec90 = Recref()$ec90,
        nid = as.double(input$nid),
        choose = input$choose,
        endtime = input$endtime
      )
  })  # reactive
  
# Observe add button for dynamic ui
# * Update values in `rv[[names]]` using existing input boxes using `index`
# * Add 1 to the `rv$n` if below the maximum (3)
  observeEvent(input$add, {
    names <- c("amt", "int", "dur")
    index <- seq(1, rv$n, by = 1)
    purrr::walk(names, ~ { rv[[.x]][index] <- Rinput()[[.x]][index] })
    if (rv$n < 3) { rv$n <- rv$n + 1 }
  })  # observeEvent
  
# Observe remove button for dynamic ui
# * Update values in `rv[[names]]` using existing input boxes using `index`
# * Subtract 1 from the `rv$n` if above the minimum (1)
  observeEvent(input$rem, {
    names <- c("amt", "int", "dur")
    index <- seq(1, rv$n, by = 1)
    purrr::walk(names, ~ { rv[[.x]][index] <- Rinput()[[.x]][index] })
    if (rv$n > 1) { rv$n <- rv$n - 1 }
  })  # observeEvent
  
# Observe values of input$choose for dynamic ui
# * Based on the selection, update the value of `rv$n` based on desired preset
# * Using the new `rv$n` value to define the index, update `rv` dosing values
  observeEvent(input$choose, {
    rv$reg <- input$choose
    rv$n <- dplyr::case_when(
      as.double(input$choose) %in% c(1, 3, 4) ~ 1,
      as.double(input$choose) == 2 ~ 2,
      as.double(input$choose) == 0 ~ rv$n)
    index <- seq(1, rv$n, by = 1)
    if (input$choose == "1") {  # "Days 1-3: 500 mg" = 1,
      rv$amt[index] <- 500
      rv$int[index] <- 24
      rv$dur[index] <- 3
    } else if (input$choose == "2") {  # "Day 1: 500mg; Days 2-5: 250 mg" = 2,
      rv$amt[index] <- c(500, 250)
      rv$int[index] <- c(24, 24)
      rv$dur[index] <- c(1, 4)
    } else if (input$choose == "3") {  # "Day 1: 1000mg" = 3,
      rv$amt[index] <- 1000
      rv$int[index] <- 24
      rv$dur[index] <- 1
    } else if (input$choose == "4") {  # "Days 1-10: 500mg" = 4
      rv$amt[index] <- 500
      rv$int[index] <- 24
      rv$dur[index] <- 10
    }  # end if
  })  # observeEvent
  
# Return dynamic ui as output
  output$regimen <- renderUI(Rui())
  
# Call module responsible for simulation and pass in inputs
# Assign output from module to rsim
  rsim <- callModule(mod_simulate_server, "sim", Rinput = Rinput, rv = rv)
  
# Return rsim object (from mod_simulate_server) to parent (app_server) 
  return(rsim)
  
}  # mod_regimen_server
