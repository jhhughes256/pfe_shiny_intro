# ui.r (Interactive User-Interface) -------------------------------------------
ui <- fluidPage(
# Application Title and Logo
  fixedRow(
    h2("My PopPK App"),
    align = "center"
  ),	# fixedRow
# Add a break with a horizontal line
  hr(),
# Begin sidebar layout
  sidebarLayout(
  # Sidebar panel to contain the inputs
    sidebarPanel(  # left panel
      h4("Dosing Regimen"),
      numericInput("DOSEAMT",  # treatment dose
        label = "Dose (mg):",
        min = 0, max = NA, value = 100
      ),  # numericInput
      selectInput("DOSEFRQ",  # treatment frequency
        label = "Dosage Frequency",
        choices = list(
          "once daily" = 24,
          "twice daily" = 12,
          "three times daily" = 8),
        selected = "once daily"
      ),  # selectInput
      sliderInput("DOSEDUR",  # treatment duration
        label = "Treatment Duration (days)",
        min = 1, max = 21, value = 3, step = 1
      )  # sliderInput
    ),  # sidebarPanel
  # Main panel to contain the outputs - using tabs
    mainPanel(  # right panel
      plotOutput("pkplot")
    )	 # mainPanel
  )	 # sidebarLayout
)
