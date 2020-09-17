# ui.R script for Michaelis-Menten model app
# ------------------------------------------------------------------------------
  fixedPage(
    h3("Michaelis-Menten Model"),
    hr(),
    plotOutput("mmPlot", width = 600),
    fluidRow(
      column(6,
        numericInput("Vmax",
          "Maximum Rate (Vmax):",
          value = 100
        ),
        numericInput("km",
          "Michaelis constant (Km):",
          value = 50
        ),
        numericInput("Cmax",
          "Maximum value on x-axis:",
          value = 400
        )
      ),
      column(6,
        numericInput("A",
          "Amount of drug in tissue",
          value = 100
        ),
        numericInput("V",
          "Volume of tissue",
          value = 2
        )
      )
    ),
    align = "center"
  )  # fixedPage
