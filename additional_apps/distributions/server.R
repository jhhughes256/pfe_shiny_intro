# Server script for a Shiny application demonstrating different distributions
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/
# -----------------------------------------------------------------------------
# Non-reactive expressions
# Can be placed in separate .R file named "global.R"

# Load libraries
  library(shiny)
  library(ggplot2)

# Customise ggplot2 theme
  theme <- theme_set(theme_bw(base_size = 18))

# -----------------------------------------------------------------------------
# Reactive expressions
shinyServer(function(input, output) {
# Generate a sequence of x suitable for user-selected distribution
# Also dependent on the value of the "lengthx" widget from the ui.R
# These are named using the id given in the ui.R with "input$" as a prefix
# Eg. input$disttype & input$lengthx
# This results in a reactive object which can be called elsewhere in the server
# To use the reactive object it should be named as "Rx()"
  Rx <- reactive({
    if (input$disttype == 1) {
      x <- seq(from = -10, to = input$lengthx, by = 0.001)
    } else if (input$disttype == 2) {
      x <- seq(from = 0.001, to = input$lengthx, by = 0.001)
    } else {
      x <- seq(from = 0, to = input$lengthx, by = 0.001)
    }
    return(x)
  })  # reactive.Rx

# Generate the user-selected distribution
# This reactive object depends on multiple inputs from ui.R
# It also depends on the value of Rx
# Remember to use the value Rx is must be named as "Rx()"
  Rdist <- reactive({
    # Generate the normal distribution for each value of x given the input for
    # mean ("input$nmean") and standard deviation ("input$sd")
    if (input$disttype == 1) {
      dist <- 1/(sqrt(2*pi)*input$sd)*exp(-((Rx()-input$nmean)^2)/(2*input$sd^2))
    }
    # Generate the log-normal distribution for each value of x given the input
    # for mean ("input$lnmean") and standard deviation ("input$sd")
    if (input$disttype == 2) {
      dist <- 1/(sqrt(2*pi)*input$sd)*exp(-((log(Rx())-log(input$lnmean))^2)/(2*input$sd^2))
    }
    # Generate the inverse gamma distribution for each value of x given the
    # input for alpha ("input$alpha") and beta ("input$beta")
    if (input$disttype == 3) {
      dist <- (input$beta^input$alpha)/gamma(input$alpha)*Rx()^(-input$alpha-1)*exp(-input$beta/Rx())
    }
    # Generate the cauchy distribution for each value of x given the input for
    # l (location) and s (scale)
    if (input$disttype == 4) {
      dist <- 1/(pi*input$s*(1+((Rx()-input$l)/input$s)^2))
    }
    # Generate the uniform distribution for each value of x given the input
    # for min and max
    if (input$disttype == 5) {
      dist <- Rx()/Rx()*1/(input$max-input$min)
    }
    return(dist)
  })  # reactive.Rdist

# Generate the server output to be sent back to the user interface
# The render* family of functions allow objects of a variety of classes to be
# displayed to the user, dependent on their input.
# We want to plot the reactive x and dist values outlined above.
# Any ggplot object that works in an R script, will work in Shiny.
# Remember, these are names as "Rx()" and "Rdist()"
  output$distPlot <- renderPlot({
    plotobj1 <- NULL
    plotobj1 <- ggplot()
    plotobj1 <- plotobj1 + geom_line(aes(x = Rx(),y = Rdist()))
    plotobj1 <- plotobj1 + scale_y_continuous("Distribution Density\n")
    plotobj1 <- plotobj1 + scale_x_continuous("\nx")
    print(plotobj1)
  })  #output.distPlot
})  # shinyServer
