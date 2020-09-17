# Conversion from R-Script to Application -------------------------------------
# In the previous steps the app was updated to allow the selection of patient
#   diagnosis used to subset the data viewed in the output. This has made the
#   application slower, particularly when clicking on the log-scale checkboxes.
#   This is because the data was being subset every time the buttons were 
#   clicked, even though the only code that needed updating the was the plot
#   code. This step shows how to structure the server so that run-time is
#   optimised for user quality of life.
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

# Define "computationally expensive" function
  slowFunction <- function(x) {
    Sys.sleep(2)
    return(x)
  }

# Set ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 10))
  theme_update(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))

# Load dataset
  load("eicu_labs.Rdata")  # creates loads R object `eicuData`
  
# Identify most common reasons for admission
  admissionChoices <- eicuData %>%
    pull(apacheadmissiondx) %>%
    unique() %>%
    as.list()
  
# Identify most common lab results
  labChoices <- eicuData %>%
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
      h2("eICU Lab Data"),
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
        h4(strong("Patient Data")),
        selectInput("admissiondx", "Admission Diagnosis",
          choices = admissionChoices,
          selected = "Infarction, acute myocardial (MI)"
        ),  # selectInput
        selectInput("plot_x", "Lab results (x-axis)",
          choices = labChoices,
          selected = "troponin - I"
        ),  # selectInput
        selectInput("plot_y", "Lab results (y-axis)",
          choices = labChoices,
          selected = "CPK-MB"
        ),  # selectInput
        h4(strong("Plot options")),
        checkboxInput("xlog", "View x-axis on log-scale"),
        checkboxInput("ylog", "View y-axis on log-scale")
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
# The reactive function can be used to define reactive objects, which can be 
#   used by other pieces of server code. When doing so, they will only update
#   if the inputs it depends on change. Rsubdf is a reactive object dependent
#   on input$admissiondx. Therefore when other inputs change, the application
#   will no longer run the subset code, making the application faster.
# A reactive object, Rsubdf, can be thought of as a function with no arguments.
#   Therefore to access the content of the object you would type: Rsubdf()
#   All code that leads to creating that content is contained in { } like when
#   using renderPlot.
  app_server <- function(input, output) {
    
  # The subset code has been separated from the plotting code and placed within
  #   a reactive function. The subdf object within the { } is output when Rsubdf()
  #   is included in another piece of code in the server. This code is only 
  #   dependent on the diagnosis input, so it won't run when changing values in
  #   the plot.
    RsubsetData <- reactive({
    # Read inputs from UI
      admissionDx <- input$admissiondx
    # Create subset of the data (copy-pasted code)
      subsetData <- eicuData %>%
        filter(apacheadmissiondx == admissionDx) %>%
        slowFunction()
    })
    
  # The renderPlot function no longer includes the subset code, instead it calls
  #   the output of the subset reactive object, Rsubdf. Therefore the plot is
  #   dependent on the four plot inputs as well as the Rsubdf reactive object.
  #   The plot will be updated whenever any of these dependencies change.
    output$plot <- renderPlot({
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
      xlog <- input$xlog
      ylog <- input$ylog
    # Read intermediate objects from server
      subsetData <- RsubsetData()
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
      p <- p + labs(x = paste0(plot_x, " (", xunit, ")"), y = paste0(plot_y, " (", yunit, ")"))
      if (xlog) {
        p <- p + scale_x_log10()
      }
      if (ylog) {
        p <- p + scale_y_log10()
      }
      p
    })
    
  }  # app_server
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Run application
  shinyApp(app_ui, app_server)