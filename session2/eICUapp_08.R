# Global environment (Non-Reactive Server Objects) ----------------------------
# Load package libraries
  library(shiny)
  library(tidyverse)

# Set ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 14))
  theme_update(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))

# Load dataset
  load("eicu_labs.Rdata")  # creates loads R object `eicuData`

# UI function (Interactive User-Interface) ------------------------------------
  app_ui <- fluidPage(
  # Title banner
    titlePanel("eICU Lab Data"),
    div(
      p(paste("Lab data sourced from open-access database eICU Collaborative", 
        "Research Database Demo.")),
      p(em(paste("Johnson, A., Pollard, T., Badawi, O., & Raffa, J. (2019).", 
        "eICU Collaborative Research Database Demo (version 2.0). PhysioNet.")), 
        a("doi.org/10.13026/gxmm-es70", 
          href = "https://physionet.org/content/eicu-crd-demo/2.0/"))
    ),
    br(),
  # Sidebar Layout
    sidebarLayout(
    # Left panel (sidebar, inputs)
      sidebarPanel(
        h4("Lab Data Options"),
        selectInput("diagnosis", "Patient Diagnosis",
          choices = list("All Diagnoses", 
            "Cardiac arrest", "CHF, congestive heart failure", 
            "CVA, cerebrovascular accident/stroke", "Diabetic ketoacidosis", 
            "Emphysema/bronchitis", "Infarction, acute myocardial (MI)", 
            "Pneumonia, bacterial", "Rhythm disturbance (atrial, supraventricular)", 
            "Sepsis, pulmonary", "Sepsis, renal/UTI (including bladder)"),
          selected = "All Diagnoses"
        ),
        selectInput("plot_x", "X-axis",
          choices = list("albumin", "alkaline phos.", "ALT (SGPT)", 
            "AST (SGOT)", "bicarbonate", "BUN", "calcium", "Carboxyhemoglobin", 
            "chloride", "CPK", "CPK-MB", "creatinine", "glucose", "Hct", "Hgb", 
            "lactate", "magnesium", "MCHC", "MCV", "paCO2", "paO2", "phosphate", 
            "platelets x 1000", "potassium", "PT - INR", "PTT", "RBC", "sodium", 
            "total bilirubin", "total protein", "troponin - I", "WBC x 1000"),
          selected = "troponin - I"
        ),  # selectInput
        selectInput("plot_y", "Y-axis",
          choices = list("albumin", "alkaline phos.", "ALT (SGPT)", 
            "AST (SGOT)", "bicarbonate", "BUN", "calcium", "Carboxyhemoglobin", 
            "chloride", "CPK", "CPK-MB", "creatinine", "glucose", "Hct", "Hgb", 
            "lactate", "magnesium", "MCHC", "MCV", "paCO2", "paO2", "phosphate", 
            "platelets x 1000", "potassium", "PT - INR", "PTT", "RBC", "sodium", 
            "total bilirubin", "total protein", "troponin - I", "WBC x 1000"),
          selected = "CPK-MB"
        ),  # selectInput
        checkboxInput("view_table", "View individual data", value = FALSE),
        hr(),
        h4("Plot Options"),
        sliderInput("alpha", "Data point transparency",
          value = 0.5, min = 0, max = 1, step = 0.1),
        checkboxInput("log_x", "Change x-axis to log-scale", value = FALSE),
        checkboxInput("log_y", "Change y-axis to log-scale", value = FALSE)
      ),  # sidebarPanel
    # Right panel (main area, outputs)
      mainPanel(
        plotOutput("plot"),
        br(),
        dataTableOutput("table")
      )  # mainPanel
    )  # sidebarLayout
  )  # fluidPage

# Server function (Reactive Server Objects) -----------------------------------
  app_server <- function(input, output) {
    
  # Define selected diagnoses
    selectedDiagnoses <- reactive({
    # Read inputs from UI
      diagnosis <- input$diagnosis
    # Define diagnoses to display
      if (diagnosis == "All Diagnoses") {
        unique(eicuData$apacheadmissiondx)
      } else {
        diagnosis
      }
    })
    
  # Define output data
    outputData <- reactive({
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
      log_x <- input$log_x
      log_y <- input$log_y
    # Read reactive object from server
      selectedDiagnoses <- selectedDiagnoses()
    # Subset the eicu data
      subsetData <- eicuData %>%
        filter(apacheadmissiondx %in% selectedDiagnoses)
    # Create data for exploratory graphics
      outputData <- tibble(
        diagnosis = pull(subsetData, apacheadmissiondx),
        xaxis = pull(subsetData, paste0("labresult_", plot_x)), 
        yaxis = pull(subsetData, paste0("labresult_", plot_y)),
        xunit = pull(subsetData, paste0("labmeasurenamesystem_", plot_x)),
        yunit = pull(subsetData, paste0("labmeasurenamesystem_", plot_y))) 
    # Return outputData filtered for NA values
      filter(outputData, !is.na(xaxis) & !is.na(yaxis)) 
    })
    
  # Define plot output
    output$plot <- renderPlot({
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
      log_x <- input$log_x
      log_y <- input$log_y
      diagnosis <- input$diagnosis
    # Read reactive object from server
      selectedDiagnoses <- selectedDiagnoses()
      plotData <- outputData()
    # Define xunit and yunit
      xunit <- unique(plotData$xunit)
      yunit <- unique(plotData$yunit)
    # Create exploratory graphics
      p <- NULL
      p <- ggplot(plotData)
      p <- p + geom_point(aes(x = xaxis, y = yaxis, colour = diagnosis),
        alpha = input$alpha)
      p <- p + labs(
        x = paste0(plot_x, " (", xunit, ")"), 
        y = paste0(plot_y, " (", yunit, ")"),
        colour = "Diagnosis")
      if (length(selectedDiagnoses) == 1) {
        p <- p + guides(colour = "none")
      }
      if (log_x == TRUE) {
        p <- p + scale_x_log10()
      }
      if (log_y == TRUE) {
        p <- p + scale_y_log10()
      }
      p
    })
    
  # Define plot output
    output$table <- renderDataTable(options = list(pageLength = 10), {
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
      view_table <- input$view_table
    # Read reactive object from server
      tableData <- outputData()
    # If user selects to view the table...
      if (view_table == TRUE) {
      # Define xunit and yunit
        xunit <- unique(tableData$xunit)
        yunit <- unique(tableData$yunit)
      # Create output data table
        tableData %>%
          set_names(c("Diagnosis", 
            paste0(plot_x, " (", xunit, ")"), 
            paste0(plot_y, " (", yunit, ")"), 
            "xunit", "yunit")) %>%
          select(-xunit, -yunit)
      }
    })
    
  }  # app_server
  
# Run application -------------------------------------------------------------
  shinyApp(app_ui, app_server)
  