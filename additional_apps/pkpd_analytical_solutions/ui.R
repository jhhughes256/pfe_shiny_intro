# ui.r (Interactive User-Interface) -------------------------------------------
# Here the user-interface is defined, along with what can be interacted with.
#   This includes creating the layout of the app (much like organising the 
#   layout of a webpage), defining the interactive "widgets" that provide the 
#   inputs to the server and specifying where output should be shown when 
#   calculated by the server.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Specify application ui ------------------------------------------------------
app_ui <- fluidPage(
# Application Title and Logo
  fixedRow(
    h2("PKPD Example App"),
    align = "center"
  ),	# fixedRow
# Add a break with a horizontal line
  hr(),
# Begin sidebar layout
  sidebarLayout(
  # Sidebar panel to contain the inputs
    sidebarPanel(  # left panel
      h4("Dosing Regimen"),
      numericInput("NID",
        label = "Number of Individuals",
        min = 0, max = 1000, value = 1),
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
        min = 1, max = 7, value = 3, step = 1
      ),  # sliderInput
      selectInput("FOOD",
        label = "Fed status",
        choices = list(
          "Fasted" = 0,
          "Fed" = 1
        ), selected = 0),
    # Also adding an action button to control simulation
    # A submit button could be used, but this provides additional flexibility
      actionButton("sim","Simulate")
    ),  # sidebarPanel
  # Main panel to contain the outputs - using tabs
    mainPanel(  # right panel
      tabsetPanel(
        tabPanel("PK",
          plotOutput("pkplot")
        ),  # tabPanel
        tabPanel("PD",
          plotOutput("pdplot")
        )  # tabPanel
      )  # tabsetPanel
    )	 # mainPanel
  )	 # sidebarLayout
)	 # fixedPage
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 