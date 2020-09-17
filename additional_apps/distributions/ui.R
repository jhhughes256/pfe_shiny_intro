# UI script for a Shiny application demonstrating different distributions
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/
# -----------------------------------------------------------------------------

fluidPage(
# Application title
  titlePanel("Distributions"),
# The positioning function sidebarLayout sets up the UI to have a sidbarPanel
# and a mainPanel
  sidebarLayout(
  # The sidebarPanel function contains UI elements that will be featured
  # in the sidebar
    sidebarPanel(
    # selectInput is a widget that gives a drop down box with the options
    # provided to the choices argument. The name of the options correlate with
    # a number to be used in the server
      selectInput("disttype",
        "Distribution type",
        choices = list(
          "Normal" = 1,
          "Log-Normal" = 2,
          "Inverse Gamma" = 3,
          "Cauchy" = 4,
          "Uniform" = 5
        )  # choices.disttype
      ),  # selectInput.disttype
    # sliderInput is a wdiget that gives a movable slider to choose a value.
    # The min and max state the limits of the slider bar, while the step
    # defines the minimum increment.
      sliderInput("lengthx",
        "Length of x:",
        min = 0,
        max = 10,
        step = 1,
        value = 10
      ),  # sliderInput.lengthx
    # Conditional panel allows UI elements to become visible depending on the
    # answer to a logical question. Below that question is:
    # Is the selectInput widget named 'disttype' set to 1 (Normal)?
      conditionalPanel(condition = "input.disttype == 1",
        sliderInput("nmean",
          "Mean:",
          min = -10,
          max = 10,
          step = 0.1,
          value = 0
        )  # sliderInput.nmean
      ),  # conditionalPanel.disttype==1
      conditionalPanel(condition = "input.disttype == 2",
        sliderInput("lnmean",
          "Mean:",
          min = 0.1,
          max = 20,
          step = 0.1,
          value = 1
        )  # sliderInput.lnmean
      ),  # conditionalPanel.disttype==2
      conditionalPanel(condition = "input.disttype <= 2",
        sliderInput("sd",
          "Standard Deviation:",
          min = 0,
          max = 10,
          step = 0.1,
          value = 1
        )  # sliderInput.sd
      ),  # conditionalPanel.disttype<=2
      conditionalPanel(condition = "input.disttype == 3",
        sliderInput("alpha",
          "alpha:",
          min = 0.1,
          max = 2,
          step = 0.1,
          value = 0.1
        ),  # sliderInput.alpha
        sliderInput("beta",
          "beta:",
          min = 0.1,
          max = 2,
          step = 0.1,
          value = 0.1
        )  # sliderInput.beta
      ),  # conditionalPanel.disttype==3
      conditionalPanel(condition = "input.disttype == 4",
        sliderInput("l",
          "Location:",
          min = 0.1,
          max = 2,
          step = 0.1,
          value = 0.1
        ),  # sliderInput.l
        sliderInput("s",
          "Scale:",
          min = 0.1,
          max = 2,
          step = 0.1,
          value = 0.1
        )  # sliderInput.s
      ),  # condtionalPanel.disttype==4
      conditionalPanel(condition = "input.disttype == 5",
        sliderInput("min",
          "Minimum:",
          min = 0,
          max = 10,
          step = 0.5,
          value = 1
        ),  # sliderInput.min
        sliderInput("max",
          "Maximum:",
          min = 0,
          max = 10,
          step = 0.5,
          value = 7
        )  # sliderInput.max
      )  # conditionalPanel.disttype==5
    ),  # sidebarPanel
  # The mainPanel function works like the sidebarPanel function, except for
  # presents the elements within its brackets in the main body. We want to show
  # a plot of the generated distribution in the main body.
    mainPanel(
      plotOutput("distPlot")
    )  # mainPanel
  )  # sidebarLayout
)  # fluidPage
