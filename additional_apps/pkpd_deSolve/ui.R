# Define UI
fixedPage(
# Application Title and Logo
  fixedRow(
    h2("PAGANZ - Shiny Example App"),
    align = "center"
  ),	#fixedRow

# Add a break with a horizontal line
  hr(),

# Sidebar panel with widgets
  sidebarLayout(
    sidebarPanel(
    # Covariates
      h4("Patient Information"),
    # Slider input for age
      numericInput("age",
        "Age (years):",
        min = 0, max = 100, value = 32
      ),  #numericInput
    # Slider input for weight
      numericInput("wt",
        "Total body weight (kg):",
        min = 0, max = 150, value = 70
      ),  #numericInput
    # Select Input for gender
      selectInput("sex",
        "Gender:",
        choices = list(
          "Male" = 1,
          "Female" = 0
        ),
        selected = 1
      ),  #selectInput
    # Numeric input for serum creatinine
      numericInput("secr",
        "Serum creatinine (µmol/L):",
        min = 0, max = 400, value = 60, step = 1
      ),  #numericInput
    # Display creatinine clearance from input
      textOutput("crcl"),
      br(),
    # Radio buttons for smoking status
      radioButtons("smok",
        "Smoking Status:",
        choices = list(
          "Not a Current Smoker" = 0,
          "Current Smoker" = 1
        ),
        selected = 1
      ),  #radioButtons
      hr(),
      h4("Simulation Options"),
    # Radio buttons for number of individuals
      radioButtons("n",
        "Number of Individuals:",
        choices = list(
          "10" = 10,
          "100" = 100
        ),
        selected = 10,
        inline = TRUE
      ),  #radioButton
      checkboxInput("logscale", "Plot concentrations on a log-scale", value = FALSE),	#checkboxInput
      selectInput("ci",
        "Prediction Intervals:",
        choices = list(
          "80% (10th - 90th percentiles)" = 1,
          "90% (5th - 95th percentiles)" = 2
        )
      )	#selectInput
    ),  #sidebarPanel

  # Main panel to contain the concentration versus time plot
    mainPanel(
    # Plot output for concentration-time profile
      fixedRow(
        plotOutput("plotconc")
      ),	#fixedRow
      hr(),
      fixedRow(
        column(4,
        # IV Infusion Dosing
          checkboxInput("infcheck", "IV Infusion Dosing", FALSE),
          conditionalPanel(condition = "input.infcheck == true",
          # Slider input for IV infusion dose
            sliderInput("infdose",
              "Dose (mg):",
              min = 0, max = 1000, value = 500, step = 100
            ),  #sliderInput
          # Slider input for IV infusion duration
            radioButtons("infdur",
              "Duration (hours):",
              choices = list(
                "2 hours" = 2,
                "12 hours" = 12),
              selected = 2,
              inline = TRUE
            ),  #sliderInput
          # Numeric input for IV infusion starting time
            numericInput("inftimes",
              "Start Time (hours):",
              min = 0, max = 120, value = 72, step = 1
            ),  #numericInput
          # Text output for IV infusion rate
            textOutput("infrate")
          )	#conditionalPanel
        ),	#column
        column(4,
        # Oral Dosing
          checkboxInput("pocheck", "Oral Dosing", value = FALSE),
          conditionalPanel(condition = "input.pocheck",
          # Slider input for oral dose to be given every 24 hours
            sliderInput("podose",
              "Dose (mg):",
              min = 0, max = 1000, value = 500, step = 100
            ),  #sliderInput
          # Select input for oral dose frequency
            selectInput("potimes",
              "Dosing Frequency:",
              choices = list(
                "Once daily (every 24 hours)" = 1,
                "Twice daily (every 12 hours)" = 2,
                "Three times a day (every 8 hours)" = 3,
                "Four times a day (every 6 hours)" = 4
              ),
              selected = 1
            ),  #selectInput
          # Numeric input for start time of oral dosing
            numericInput("postart",
              "Start Time (hours):",
              min = 0, max = 120, value = 0, step = 1
            )	#numericInput
          )	#conditionalPanel
        ),	#column
        column(4,
        # IV Bolus Dosing
          checkboxInput("ivcheck", "IV Bolus Dosing", FALSE),
          conditionalPanel(condition = "input.ivcheck == true",
          # Slider input for IV bolus dose
            sliderInput("ivdose",
              "Dose (mg):",
              min = 0, max = 1000, value = 500, step = 100
            ),  #sliderInput
          # Slider input for IV bolus dose time
            numericInput("ivtimes",
              "Dose Time (hours):",
              min = 0, max = 120, value = 0
            )  #numericInput
          )	#conditionalPanel
        )	#column
      )	#fixedRow
    )	 #mainPanel
  )	#sidebarLayout
)	#fixedPage
