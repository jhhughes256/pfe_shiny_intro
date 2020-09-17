#' @import shiny shinydashboard
app_ui <- function() {
# Define ui functions 
  dashboardPage(
  # Header  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    header = dashboardHeader(title = "Azithromycin PK Simulator", titleWidth = 300),
  # Sidebar - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Simulate", tabName = "simulate", icon = icon("line-chart")),
        menuItem("About", tabName = "about", icon = icon("question-circle"),
          menuSubItem("Documentation", tabName = "documentation", icon = icon("file")),
          menuSubItem("Model", tabName = "model", icon = icon("code"))#,
          # menuSubItem("Resources", tabName = "resources", icon = icon("desktop"))
        ) # menuItem
      ),  # sidebarMenu
      width = 300, collapsed = TRUE
    ),  # dashboardSidebar
  # Body  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    body = dashboardBody(
      tabItems(
      # Tab 1 - Simulate
        tabItem(tabName = "simulate",
          fluidRow(  # removes shinydashboard background issues
          # Dosing Input
            div(class = "col-sm-6 col-lg-4",  # define layout widths
              mod_regimen_ui("reg")
            ),  # div
          # Model Output
            div(class = "col-sm-6 col-lg-8",  # define layout widths
              mod_plotsim_ui("poppk")
            )  # div
          )  # fluidRow
        ), # tabItem
      # Tab 2.1 - Documentation
        tabItem(tabName = "documentation",
          mod_infotab_ui("doc")
        ), # tabItem
      # Tab 2.2 - Model
        tabItem(tabName = "model",
          mod_infotab_ui("model", pre)
        )#, # tabItem
      # Tab 2.3 - Resources
        # tabItem(tabName = "resources",
        #   mod_infotab_ui("session", pre)
        # ) # tabItem
      ) # tabItems
    )  # dashboardBody
  )  # dashboardPage
}  # app_ui