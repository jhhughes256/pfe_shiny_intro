# Conversion from R-Script to Application -------------------------------------
# When using the app from the previous step, an annoying behaviour was 
#   discovered. When looking at two non-default lab-values and changing the
#   diagnosis input, the chosen lab-values would reset to the default values.
#   This step shows how to avoid this annoying behaviour, using reactiveValues
#   to "remember" what the lab-values were before the diagnosis was changed.
#   This won't help when a lab-value is not a valid choice for a given diagnosis,
#   but should help for the majority of diagnoses!
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
    ),
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
        uiOutput("labvalui"),
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
# To give the app a "memory", the reactiveValues function is used to save the
#   state of a value when an "event" occurs.
# A reactiveValues object can be thought of as a list. Therefore it is like 
#   input and output, except you may both read AND write to this list. It is 
#   often used in apps to provide values from a previous state of the 
#   application, and therefore the app has a memory.
# The values in reactiveValues change when an "event" occurs, namely the 
#   diagnosis used to subset the datset changes. observeEvent is a function that
#   observes the first argument for changes. When a change occurs (an event) it
#   will run the code within the { } in the second argument.
  app_server <- function(input, output) {
    
  # Set up a reactiveValues object to store the values that should be remembered
  # The values assigned to items in the list are the initial conditions of the 
  #   reactiveValues object. Therefore, they should be the values that were
  #   originally provided to the selected arguments in the selectInput functions
  #   in output$labvalui.
    RVlab <- reactiveValues(
      init_x = "troponin - I",
      init_y = "CPK-MB"
    )
    
  # Set up an observer on the diagnosis input, input$admissiondx. When this value
  #   changes the reactive values object, RVlab, is updated with the selected
  #   lab values before the UI is updated. Therefore when the UI is updated,
  #   the selected argument will be provided the lab values that were selected
  #   before the diagnosis input was changed.
    observeEvent(input$admissiondx, {
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
    # Update RVlab to the selected values provided that the UI exists
    # What this if statement does, is it stops RVlab from being updated to NULL
    #   when the dynamic UI hasn't been rendered yet! 
      if (!is.null(plot_x) | !is.null(plot_y)) {
        RVlab$init_x <- plot_x
        RVlab$init_y <- plot_y
      }
    })
    
    RsubsetData <- reactive({
    # Read inputs from UI
      admissionDx <- input$admissiondx
    # Create subset of the data (copy-pasted code)
      subsetData <- eicuData %>%
        filter(apacheadmissiondx == admissionDx) %>%
        slowFunction()
    })
    
    RlabChoices <- reactive({
    # Read intermediate objects from server
      subsetData <- RsubsetData()
    # Identify most common lab results for given diagnosis
      labChoices <- subsetData %>%
        select(matches("^labresult_")) %>%
        map(function(x) sum(!is.na(x))) %>%
        magrittr::extract(. > 50) %>%
        names() %>%
      # Clean up each of the lab result names and order alphabetically in list form
        str_remove("^labresult_") %>%
        magrittr::extract(str_order(.)) %>%
        as.list()
    })
    
  # This output is updated so that the selected argument is no longer hard-coded
  #   but instead reads values from the RVlab reactiveValue object.
    output$labvalui <- renderUI({
    # Read intermediate objects from server
      labChoices <- RlabChoices()
      init_x <- RVlab$init_x
      init_y <- RVlab$init_y
    # Create lab value UI
      tagList(
        selectInput("plot_x", "Lab results (x-axis)",
          choices = labChoices,
          selected = init_x
        ),  # selectInput
        selectInput("plot_y", "Lab results (y-axis)",
          choices = labChoices,
          selected = init_y
        )  # selectInput
      )
    })
    
    output$plot <- renderPlot({
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
      xlog <- input$xlog
      ylog <- input$ylog
    # Read intermediate objects from server
      subsetData <- RsubsetData()
    # As plot_x and plot_y are now dependent on other UI (defined in the server), 
    #   need to account for when they don't exist (app-startup) to prevent errors.
      if (!is.null(plot_x) | !is.null(plot_y)) {
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
      }
    })
    
  }  # app_server
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Run application
  shinyApp(app_ui, app_server)