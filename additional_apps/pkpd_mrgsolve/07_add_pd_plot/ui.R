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
      sliderInput("NID",
        label = "Number of Individuals",
        value = 1, min = 1, max = 1000
      ),  # numericInput
      numericInput("COVBWT",
        label = "Average Body Weight (kg)",
        value = 70
      ),  # numericInput
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
      ),  # sliderInput
      h4("Plot Options"),
      checkboxInput("logscale", "Plot y-axis on log-scale", value = FALSE),
      actionButton("sim", "Simulate")
    ),  # sidebarPanel
  # Main panel to contain the outputs - using tabs
    mainPanel(  # right panel
    # Tabs are implemented using the tabsetPanel UI function. It's main arguments
    #   are tabPanel functions for each tab in the app. The tabPanel functions
    #   have a title argument, which controls the text on the tab UI which should
    #   then be followed by the UI code to be shown when the tab is selected.
    # Note: Shiny is lazy (good thing), so outputs that aren't displayed (on a
    #   tab which isn't selected) aren't computed. They'll only be computed
    #   when you switch over to that tab. To see this in action, switch to the
    #   the PD tab and then back to the PK tab. Change the simulation options
    #   and press the simulation button. Once the PK plot has updated, click the
    #   PD tab and you'll see the old plot briefly before shiny computes the
    #   updated plot!
      tabsetPanel(
      # PK Tab
        tabPanel(title = "PK",
        # PK Plot
          plotOutput("pkplot"),
          column(6, 
            h4("Single Dose PK"),
            tableOutput("pksdtable")
          ),  # column
        # NCA Tables
          column(6, 
            h4("Steady-State PK"),
            tableOutput("pksstable")
          ),  # column
        # Caption
          div(
            p(paste("AUCtau - area under the concentration curve for the dosing", 
              "interval; Cave - average concentration for the dosing interval;",
              "Cmax - maximum concentration for the dosing interval; Ctrough -",
              "trough concentration for the dosing interval; 90% PI - 90%",
              "prediction intervals (5th and 95th percentiles)."
            )),  # p
            align = "justify"
          )  # div
        ),  # tabPanel
      # PD Tab
        tabPanel(title = "PD",
          plotOutput("pdplot")
        )  # tabPanel
      ),  # tabsetPanel
      align = "center"
    )	 # mainPanel
  )	 # sidebarLayout
)	 # fixedPage

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 