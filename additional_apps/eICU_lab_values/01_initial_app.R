# Conversion from R-Script to Application -------------------------------------
# In this step the inputs from the original script are considered, and the UI
#   is prototyped prior to any server code being written. The non-reactive parts
#   of the original script (loading packages, datasets and subsetting) are
#   defined prior to defining the ui and server as they only need to run once.
#   The output code from the original script is added to the server. This
#   code accesses the globally available data and the input from the UI to
#   define the output plot for the UI to display.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Global environment (Non-Reactive Server Objects) ----------------------------
# All non-reactive components of your application are specified here. This 
#   consists of any application dependencies like your model specification,
#   packages, additional functions, data, etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Set up global environment ---------------------------------------------------
# Load package libraries
  library(shiny)
  library(tidyverse)

# Set ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 10))
  theme_update(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))

# Load dataset
  load("eicu_labs.Rdata")  # creates loads R object `eicuData`
  
# Select input from this list
  admissionDx <- "Infarction, acute myocardial (MI)"
  
# Create subset of the data
  subsetData <- eicuData %>%
    filter(apacheadmissiondx == admissionDx)
  
# Identify most common lab results
  labChoices <- subsetData %>%
    select(matches("^labresult_")) %>%
    map(function(x) sum(!is.na(x))) %>%
    magrittr::extract(. > 50) %>%
    names() %>%
  # Clean up each of the lab result names and order alphabetically in list form
    str_remove("^labresult_") %>%
    magrittr::extract(str_order(.)) %>%
    as.list()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# UI function (Interactive User-Interface) ------------------------------------
# Here the user-interface is defined, along with what can be interacted with.
#   This includes creating the layout of the app (much like organising the 
#   layout of a webpage), defining the interactive "widgets" that provide the 
#   inputs to the server and specifying where output should be shown when 
#   calculated by the server.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Specify application ui ------------------------------------------------------
  app_ui <- fluidPage(
  # Application title and subtitle
    fluidRow(
      h2("eICU Lab Data - Acute Myocardial Infarction"),
      p(em(paste("Lab data sourced from open-access database eICU Collaborative", 
        "Research Database Demo."))),
      p(em(paste("Johnson, A., Pollard, T., Badawi, O., & Raffa, J. (2019).", 
        "eICU Collaborative Research Database Demo (version 2.0). PhysioNet.")), 
        a("doi.org/10.13026/gxmm-es70", 
          href = "https://physionet.org/content/eicu-crd-demo/2.0/")),
      align = "center"
    ),  # fluidRow
  # Horizontal line
    hr(),
  # Sidebar Layout
    sidebarLayout(
    # Left panel (sidebar, inputs)
      sidebarPanel(
        h4(strong("Lab Results")),
        selectInput("plot_x", "X-axis",
          choices = labChoices,
          selected = "troponin - I"
        ),  # selectInput
        selectInput("plot_y", "Y-axis",
          choices = labChoices,
          selected = "CPK-MB"
        )  # selectInput
      ),  # sidebarPanel
    # Right panel (main area, outputs)
      mainPanel(
        plotOutput("plot")
      )  # mainPanel
    )  # sidebarLayout
  )  # fluidPage

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Server function (Reactive Server Objects) -----------------------------------
# All reactive components of your application are specified here. This is any
#   code from your original script that depends on the inputs that were
#   identified.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Specify server code ---------------------------------------------------------
  app_server <- function(input, output) {
    
  # Define plot output
    output$plot <- renderPlot({
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
    # Create data for exploratory graphics
      plotData <- subsetData
      names(plotData)[names(plotData) == paste0("labresult_", plot_x)] <- "xaxis"
      names(plotData)[names(plotData) == paste0("labresult_", plot_y)] <- "yaxis"
      plotData <- plotData %>%
        filter(!is.na(xaxis) & !is.na(yaxis)) %>%
        mutate(xaxis = map_dbl(xaxis, mean), yaxis = map_dbl(yaxis, mean))
      xunit <- unique(unlist(plotData[paste0("labmeasurenamesystem_", plot_x)]))
      yunit <- unique(unlist(plotData[paste0("labmeasurenamesystem_", plot_y)]))
    # Create exploratory graphics
      p <- NULL
      p <- ggplot(plotData)
      p <- p + geom_point(aes(x = xaxis, y = yaxis))
      p <- p + labs(
        x = paste0(plot_x, " (", xunit, ")"), 
        y = paste0(plot_y, " (", yunit, ")"))
      p
    })
    
  }  # app_server
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Run application
  shinyApp(app_ui, app_server)