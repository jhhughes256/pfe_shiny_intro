# Conversion from R-Script to Application -------------------------------------
# With the update to the scope of the application, the method used to determine
#   what lab values could be selected in the UI changed such that it was no 
#   longer dependent on the diagnosis. Adding that dependency back to the app
#   requires the addition of UI that is dependent on the subset data, which 
#   exists in the server. To do this, the server-dependent UI (also known as
#   dynamic UI) must be located in the server also. As this is a big change,
#   the new UI is prototyped, before making it dependent on the server.
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
      # The two selectInput functions have been cut and paste to the server 
      #   and have been replaced with this uiOutput function, which will 
      #   display the UI defined on the server.
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
# The renderUI function is much like the renderPlot function, in that it sends
#   a chunk of UI code to be output by the UI. By moving this UI to the server
#   it can be dependent on the server or other UI inputs, and therefore 
#   dynamically change. Note that whenever a dependency of renderUI changes,
#   the UI is reset to its initial value (to the selected argument for 
#   selectInput). If the value specified by the selected argument doesn't exist
#   it will default to the first value provided to the choices argument.
  app_server <- function(input, output) {
    
    RsubsetData <- reactive({
    # Read inputs from UI
      admissionDx <- input$admissiondx
    # Create subset of the data (copy-pasted code)
      subsetData <- eicuData %>%
        filter(apacheadmissiondx == admissionDx) %>%
        slowFunction()
    })
    
  # The UI code that was cut from the UI function is pasted here with the { }
  #   of a renderUI function. The dependencies aren't added yet so that the new
  #   UI can be prototyped. This is useful in this case, as an additional change
  #   was required for the app to avoid an error that is implicit with using
  #   renderUI. 
    output$labvalui <- renderUI({
      tagList(
        selectInput("plot_x", "Lab results (x-axis)",
          choices = labChoices,
          selected = "troponin - I"
        ),  # selectInput
        selectInput("plot_y", "Lab results (y-axis)",
          choices = labChoices,
          selected = "CPK-MB"
        )
      )
    })  # output$labvalui
    
  # Define plot code
  # Since the plot output is dependent on an input generated by a renderUI output,
  #   it is possible that this code will run before the UI has been created. If 
  #   this happens there is a brief error in the app. To prevent this, the code
  #   has been updated to check whether the inputs exist yet using is.null.
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