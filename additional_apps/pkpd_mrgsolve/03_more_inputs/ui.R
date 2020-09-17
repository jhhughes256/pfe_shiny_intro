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
    h2("My PopPK App"),
    align = "center"
  ),	# fixedRow
# Add a break with a horizontal line
  hr(),
# Begin sidebar layout
  sidebarLayout(
  # Sidebar panel to contain the inputs
    sidebarPanel(  # left panel
      h4("Population Characteristics"),
    # Add in covariate input for body weight as a numeric input
      numericInput("COVBWT",
        label = "Average Body Weight (kg)",
        value = 70
      ),  # numericInput
    # Add in covariate input for sex as radio buttons, this function has similar
    #   arguments and behaviour as selectInput
      radioButtons("COVSEX",
        label = "Sex",
        choices = list(
          "Male" = 0,
          "Female" = 1),
        selected = 0
      ),  # radioButtons
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
        selected = 24
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
)	 # fixedPage

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 