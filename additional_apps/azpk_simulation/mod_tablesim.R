# Module UI
  
#' @title   mod_tablesim_ui and mod_tablesim_server
#' @description  Shiny module which handles display of dosing regimen text and
#' table of metrics. Includes data processing required to generate the table and 
#' regimen strings. Child module of mod_plotsim.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_tablesim
#'
#' @keywords internal
#' @export 
#' @import shiny

mod_tablesim_ui <- function(id) {
# Define namespace function for IDs
  ns <- NS(id)
# Create tagList to be used in the UI
  tagList(
    div(class = "col-sm-12 col-md-6", align = "center",
      uiOutput(ns("regout")),
      tableOutput(ns("tabout"))
    ),  # div
    div(class = "col-sm-12 col-md-6", align = "center",
      uiOutput(ns("regsave")),
      tableOutput(ns("tabsave"))
    ),  # div
    column(12,
      p(paste("Abbreviations: AM - alveolar macrophage; PI - prediction interval;", 
      "EC50: 50% effective concentration; EC90: 90% effective concentration;",
      "WBC: white blood cell predictions for polymorphonuclear leukocytes,",
      "mononuclear leukocytes and peripheral blood monocytes."))
    )  # column
  )  # tagList
}  # mod_tablesim_ui

# Module Server
    
#' @rdname mod_tablesim
#' @export
#' @keywords internal

mod_tablesim_server <- function(input, output, session, rsim) {
# Define namespace function for IDs
  ns <- session$ns
  
# Render dose regimen text to be displayed in the application UI
  output$regout <- renderUI({
    fct_process_regimentext(rsim$out_reg, rsim$save_reg)
  })
  output$regsave <- renderUI({
    fct_process_regimentext(rsim$save_reg, rsim$out_reg)
  })

# Render tables to be displayed in the application
  output$tabout <- renderTable({
    fct_process_table(rsim$out, "Current Regimen")
  })
  output$tabsave <- renderTable({
    fct_process_table(rsim$save, "Saved Regimen")
  })

}  # mod_tablesim_server
