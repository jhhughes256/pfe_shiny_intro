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
  # Sidebar Layout
    br(),
    sidebarLayout(
    # Left panel (sidebar, inputs)
      sidebarPanel(
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
        sliderInput("alpha", "Data point transparency",
          value = 0.5, min = 0, max = 1, step = 0.1),
        checkboxInput("log_x", "Change x-axis to log-scale", value = FALSE),
        checkboxInput("log_y", "Change y-axis to log-scale", value = FALSE)
      ),  # sidebarPanel
    # Right panel (main area, outputs)
      mainPanel(
        plotOutput("plot"),
        dataTableOutput("table")
      )  # mainPanel
    )  # sidebarLayout
  )  # fluidPage

# Server function (Reactive Server Objects) -----------------------------------
  app_server <- function(input, output) {
    
  # Define plot output
    output$plot <- renderPlot({
    # Read inputs from UI
      plot_x <- input$plot_x
      plot_y <- input$plot_y
      log_x <- input$log_x
      log_y <- input$log_y
      diagnosis <- input$diagnosis
    # Define diagnoses to display
      if (diagnosis == "All Diagnoses") {
        selectedDiagnoses <- unique(eicuData$apacheadmissiondx)
      } else {
        selectedDiagnoses <- diagnosis
      }
    # Subset the eicu data
      subsetData <- eicuData %>%
        filter(apacheadmissiondx %in% selectedDiagnoses)
    # Create data for exploratory graphics
      plotData <- tibble(
          diagnosis = pull(subsetData, apacheadmissiondx),
          xaxis = pull(subsetData, paste0("labresult_", plot_x)), 
          yaxis = pull(subsetData, paste0("labresult_", plot_y)),
          xunit = pull(subsetData, paste0("labmeasurenamesystem_", plot_x)),
          yunit = pull(subsetData, paste0("labmeasurenamesystem_", plot_y))) %>% 
        filter(!is.na(xaxis) & !is.na(yaxis))
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
      log_x <- input$log_x
      log_y <- input$log_y
      diagnosis <- input$diagnosis
    # Define diagnoses to display
      if (diagnosis == "All Diagnoses") {
        selectedDiagnoses <- unique(eicuData$apacheadmissiondx)
      } else {
        selectedDiagnoses <- diagnosis
      }
    # Subset the eicu data
      subsetData <- eicuData %>%
        filter(apacheadmissiondx %in% selectedDiagnoses)
    # Create data for exploratory graphics
      tableData <- tibble(
          diagnosis = pull(subsetData, apacheadmissiondx),
          xaxis = pull(subsetData, paste0("labresult_", plot_x)), 
          yaxis = pull(subsetData, paste0("labresult_", plot_y)),
          xunit = pull(subsetData, paste0("labmeasurenamesystem_", plot_x)),
          yunit = pull(subsetData, paste0("labmeasurenamesystem_", plot_y))) %>% 
        filter(!is.na(xaxis) & !is.na(yaxis)) 
      xunit <- unique(tableData$xunit)
      yunit <- unique(tableData$yunit)
    # Create output data table
      tableData %>%
        set_names(c("Diagnosis", 
          paste0(plot_x, " (", xunit, ")"), 
          paste0(plot_y, " (", yunit, ")"), 
          "xunit", "yunit")) %>%
        select(-xunit, -yunit)
    })
    
  }  # app_server
  
# Run application -------------------------------------------------------------
  shinyApp(app_ui, app_server)
  