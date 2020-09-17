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
      )  # sliderInput
    ),  # sidebarPanel
  # Main panel to contain the outputs - using tabs
    mainPanel(  # right panel
      plotOutput("pkplot"),
    # Add in a corresponding tableOutput for each output defined with 
    #   renderTable. Two column functions are used to place the tables 
    #   side-by-side, rather than above-and-below each other. The 6 defines the
    #   width of each column, with the maximum width being 12. Therefore, each
    #   column (and therefore the elements within the column) take up half of 
    #   the available space.
      column(6, 
        h4("Single Dose PK"),
        tableOutput("pksdtable")
      ),  # column
      column(6, 
        h4("Steady-State PK"),
        tableOutput("pksstable")
      ),  # column
    # Because of limited space, the table has abbreviations. Just as with 
    #   reports it's good practice to provide a caption defining these 
    #   abbreviations for your users, but also future you! Here the 
    #   paragraph HTML tag is used, p, which is conveniently included in the
    #   Shiny package as an R function. ?shiny::builder shows which HTML tags 
    #   are loaded when using Shiny. 
    # The p function is wrapped in a div function (another HTML tag), which 
    #   allows different alignment to be given to this text, rather than being 
    #   centered like the plot, headers and tables. This is done by using the 
    #   align argument. The possible arguments for div can be found by googling 
    #   "div HTML attributes". There are many! The style attribute is also very
    #   useful if you want to deviate from the default.
    # The paste function is used within the p function as a result of 
    #   code-styling preference. This p function would work with a single (very 
    #   long) string.
      div(
        p(paste("AUCtau - area under the concentration curve for the dosing", 
          "interval; Cave - average concentration for the dosing interval;",
          "Cmax - maximum concentration for the dosing interval; Ctrough -",
          "trough concentration for the dosing interval; 90% PI - 90%",
          "prediction intervals (5th and 95th percentiles)."
        )),  # p
        align = "justify"
      ),  # div
      align = "center"
    )	 # mainPanel
  )	 # sidebarLayout
)	 # fixedPage

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 