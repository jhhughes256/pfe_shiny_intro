# server.R script for Michaelis-Menten model app
# The model takes the form of an equation describing the rate of enzymatic
# reactions, by relating reaction rate to the concentration of a substrate.
# ------------------------------------------------------------------------------
# Non-reactive objects/expressions
# Load package libraries
  library(shiny)
  library(ggplot2)
# Define custom ggplot theme
  theme_bw2 <- theme_set(theme_bw(base_size = 20))

# -----------------------------------------------------------------------------
# Reactive objects/expressions
shinyServer(function(input, output, session) {
  Rline <- reactive({
    Vmax <- input$Vmax
    km <- input$km
    C <- seq(0, input$Cmax, by = input$Cmax/100)
    data.frame(
      c = C,
      v = Vmax*C/(km+C)
    )
  })

  Rpoint <- reactive({
    Vmax <- input$Vmax
    km <- input$km
    A <- input$A
    V <- input$V
    data.frame(
      c = A/V,
      v = Vmax*(A/V)/(km+(A/V))
    )
  })

  output$mmPlot <- renderPlot({
    Vmax <- input$Vmax
    km <- input$km
    Cmax <- input$Cmax

    p <- NULL
    p <- ggplot()
    p <- p + geom_line(aes(x = c, y = v), 
      data = Rline(), colour = "blue", size = 1)
    p <- p + geom_hline(aes(yintercept = c(Vmax/2, Vmax)), 
      linetype = "dashed")
    p <- p + geom_segment(aes(x = km, y = 0, xend = km, 
      yend = Vmax/2), linetype = "dashed")
    p <- p + geom_text(aes(x = km + km/3, y = Vmax/20, 
      label = "Km"), size = 6)
    p <- p + geom_text(aes(x = Cmax*0.1, y = Vmax*0.95, 
      label = "Vmax"), size = 6)
    p <- p + geom_text(aes(x = Cmax*0.9, y = Vmax*0.45, 
      label = "0.5 x Vmax"), size = 6)
    p <- p + geom_point(aes(x = c, y = v), data = Rpoint(), 
      colour = "red", size = 4)
    p <- p + geom_text(aes(x = c+Cmax/10, y = v, 
      label = paste("v =", signif(v, 3))), data = Rpoint(), size = 6)
    p <- p + xlab("\nSubstrate Concentration [S]")
    p <- p + ylab("Reaction Rate v\n")
    p
  })

  # Close the R session when browser closes
  session$onSessionEnded(function(){
   stopApp()
  })  # session.onSessionEnded
})  # shinyServer
