# Prepare global environment --------------------------------------------------
# Load package libraries
  library(shiny)
  library(shinydashboard)

# Sidebar Layout --------------------------------------------------------------
# ui <- fluidPage(
# 
#   titlePanel("Hello Shiny!"),
# 
#   sidebarLayout(
# 
#     sidebarPanel(
#       sliderInput("obs", "Number of observations:",  
#                   min = 1, max = 1000, value = 500)
#     ),
# 
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# )

# Grid Layout -----------------------------------------------------------------
# dataset <- diamonds
#
# ui <- fluidPage(
# 
#   titlePanel("Diamonds Explorer"),
#   
#   plotOutput('plot'),
#   
#   hr(),
# 
#   fluidRow(
#     column(3,
#       h4("Diamonds Explorer"),
#       sliderInput('sampleSize', 'Sample Size', 
#                   min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
#                   step=500, round=0),
#       br(),
#       checkboxInput('jitter', 'Jitter'),
#       checkboxInput('smooth', 'Smooth')
#     ),
#     column(4, offset = 1,
#       selectInput('x', 'X', names(dataset)),
#       selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
#       selectInput('color', 'Color', c('None', names(dataset)))
#     ),
#     column(4,
#       selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
#       selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
#     )
#   )
# )

# Tabset Panel/Layout ---------------------------------------------------------
# ui <- fluidPage(
# 
#   titlePanel("Tabsets"),
# 
#   sidebarLayout(
# 
#     sidebarPanel(
#       # Inputs excluded for brevity
#     ),
# 
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plot", plotOutput("plot")),
#         tabPanel("Summary", verbatimTextOutput("summary")),
#         tabPanel("Table", tableOutput("table"))
#       )
#     )
#   )
# )
# Navlist Panel/Layout --------------------------------------------------------
# ui <- fluidPage(
#   
#   titlePanel("Application Title"),
#   
#   navlistPanel(
#     "Header A",
#     tabPanel("Component 1"),
#     tabPanel("Component 2"),
#     "Header B",
#     tabPanel("Component 3"),
#     tabPanel("Component 4"),
#     hr(),
#     tabPanel("Component 5")
#   )
# )
# Navbar Page/Layout ----------------------------------------------------------
# ui <- navbarPage("My Application",
#   tabPanel("Component 1"),
#   tabPanel("Component 2"),
#   navbarMenu("More",
#     tabPanel("Sub-Component A"),
#     tabPanel("Sub-Component B"))
# )
# Basic Dashboard Layout ------------------------------------------------------
# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody()
# )
# Additional Dashboard UI -----------------------------------------------------
# ui <- dashboardPage(skin = "purple",
#   dashboardHeader(title = "My Application"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#       menuItem("Widgets", tabName = "widgets", icon = icon("th"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       # First tab content
#       tabItem(tabName = "dashboard",
#         fluidRow(
#           box(plotOutput("plot1", height = 250)),
# 
#           box(
#             title = "Controls",
#             sliderInput("slider", "Number of observations:", 1, 100, 50)
#           ),
#           
#           valueBox("5%", "Success Rate", icon = icon("check-circle"), width = 6),
#           
#           infoBox("Fail Rate", "2%", "Details"),
#           
#           tabBox(width = 8,
#             # Title can include an icon
#             title = tagList(shiny::icon("gear"), "tabBox status"),
#             tabPanel("Tab1",
#               "Currently selected tab from first box:",
#               verbatimTextOutput("tabset1Selected")
#             ),
#             tabPanel("Tab2", "Tab content 2")
#           )
#         )
#       ),
# 
#       # Second tab content
#       tabItem(tabName = "widgets",
#         h2("Widgets tab content")
#       )
#     )
#   )
# )
# Finish App File -------------------------------------------------------------
server <- function(input, output) {
  
}

shinyApp(ui, server)