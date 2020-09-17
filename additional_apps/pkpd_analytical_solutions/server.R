# server.r (Reactive Server Objects) ------------------------------------------
# All reactive components of your application are specified here. This is any
#   code from your original script that depends on the inputs that were
#   identified.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Define server
# - input is a read-only list that contains all of the user input from the ui
#     + access values like you would a list `input$DOSEAMT`
# - output is a write-only list that sends output to the ui
#     + you cannot access the values in the server, but you can write to the list
#     + write `render...` function output to the list
#     + example: `output$myplot <- renderPlot(mygroovyplot())`
# - session is an optional argument, we use it here for quality of life
app_server <- shinyServer(function(input, output, session) {
  
# Simulation Inputs -----------------------------------------------------------
# This is a function that will generate an object that contains all of our model
#   inputs. It can be beneficial sometimes to group your inputs like this,
#   particularly when you have inputs in your original script that you haven't
#   implemented in the application yet. By having them already separated from
#   the rest of your code, it is easier to add them later.
# These inputs are accessed using `Rinput()$DOSEAMT` etc.
  Rinput <- reactive({
    list(
      NID = input$NID,
      DOSEAMT = input$DOSEAMT,
      DOSEFRQ = as.numeric(input$DOSEFRQ),  # selectInput provides character values
      DOSEDUR = input$DOSEDUR,
      FOOD = as.double(input$FOOD),
      CRCL = 70,
      NTIMEPOINTS = 240,
      PLOTSCALE = "linear"
    )
  })
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  
# Simulation Output -----------------------------------------------------------
# This is a function that will generate an object that contains the simulated
#   predictions from the model. `eventReactive` is used here to prevent 
#   simulation from occurring until an "event". In this case, the user pressing
#   the simulate button is the event.
# The ignoreNULL argument is set to FALSE so that the application triggers an
#   event on start up, the default is not to do this as it isn't always 
#   desirable, particularly for slower apps.
  Rsimdf <- eventReactive(input$sim, ignoreNULL = FALSE, {
  # Rather than editing our original script, we can just assign our values to
  #   the names we originally used.
    NID <- Rinput()$NID
    DOSEAMT <- Rinput()$DOSEAMT
    DOSEFRQ <- Rinput()$DOSEFRQ
    DOSEDUR <- Rinput()$DOSEDUR
    FOOD <- Rinput()$FOOD
    CRCL <- Rinput()$CRCL
    NTIMEPOINTS <- Rinput()$NTIMEPOINTS
    
  # Begin simulation!
    if (NID == 1) {  # population parameters if NID = 1
      par <- data.frame(
        ID = 1,
        dose = DOSEAMT,
        F1 = 1,
        CL = POPCL*(CRCL/90)^COVCRCL,
        V1 = POPV1,
        Q = POPQ,
        V2 = POPV2,
        KA = POPKA*(1+COVFOOD*FOOD),
        EMAX = POPEMAX,
        EC50 = POPEC50
      )
    } else {  # individual parameters if NID > 1
      ETA.mat <- MASS::mvrnorm(n = NID, mu = rep(0, length(diag(VARCOV))), 
        Sigma = VARCOV)
      par <- data.frame(
        ID = seq(1, NID, length.out = NID),
        dose = DOSEAMT,
        F1 = 1,
        CL = POPCL*exp(ETA.mat[,1])*(CRCL/90)^COVCRCL,
        V1 = POPV1*exp(ETA.mat[,2]),
        Q = POPQ,
        V2 = POPV2,
        KA = POPKA*exp(ETA.mat[,3])*(1+COVFOOD*FOOD),
        EMAX = POPEMAX,
        EC50 = POPEC50
      )
    }
  # Determine number of doses
    NDOSE <- 24*DOSEDUR/DOSEFRQ
  # Generate predictions
    simlst <- lapply(seq_along(par$ID), FUN = function(id) {
      input <- par[par$ID == id, ]
      df <- twocompabs_fn(input, seq(0, DOSEFRQ*NDOSE, by = DOSEFRQ*NDOSE/NTIMEPOINTS))
      df$ID <- id
      df$AMT <- 0
      df$AMT[1] <- DOSEAMT
      df$II <- 0
      df$II[1] <- DOSEFRQ
      df$ADDL <- 0
      df$ADDL[1] <- NDOSE - 1
      df$CL <- input$CL
      df$V1 <- input$V1
      df$Q <- input$Q
      df$V2 <- input$V2
      df$KA <- input$KA
      df$F1 <- input$F1
      df$EMAX <- input$EMAX
      df$EC50 <- input$EC50
      dfout <- df
      for (i in seq(1, NDOSE - 1, length.out = NDOSE - 1)) {
        CONC <- dfout[dfout$TIME >= i*DOSEFRQ, 2] + df[df$TIME <= (DOSEFRQ*NDOSE - i*DOSEFRQ), 2]
        dfout[dfout$TIME >= i*DOSEFRQ, 2] <- CONC
      }
      dfout$RESP <- emax_fn(input, dfout$CONC)
      return(dfout)
    })
    simdf <- do.call(rbind, simlst)
  })
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Server Outputs --------------------------------------------------------------
# Using the simulated output, now the output objects are defined. If two outputs
#   use the same intermediate object (like a data.frame of summary statistics)
#   consider having that object in its own `reactive` function!
  output$pkplot <- renderPlot({
  # Inputs
    simdf <- Rsimdf()
    NID <- Rinput()$NID
    PLOTSCALE <- Rinput()$PLOTSCALE
  # Code
    p <- NULL
    p <- ggplot(aes(x = TIME, y = CONC), data = simdf)
    if (NID == 1) {
      p <- p + geom_line(colour = "blue", size = 1)
    } else {
      p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
      p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
        alpha = 0.4, fill = "blue")
    }
    p <- p + labs(x = "Time (hours)", y = "Concentration (mg/L)")
    if (Rinput()$PLOTSCALE == "linear") {
      p <- p + scale_y_continuous()
    } else if (Rinput()$PLOTSCALE == "linear") {
      p <- p + scale_y_log10()
    }
    p
  })
  
  output$pdplot <- renderPlot({
    simdf <- Rsimdf()
    NID <- Rinput()$NID
    p <- NULL
    p <- ggplot(aes(x = TIME, y = RESP), data = simdf)
    if (NID == 1) {
      p <- p + geom_line(colour = "blue", size = 1)
    } else {
      p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
      p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
        alpha = 0.4, fill = "blue")
    }
    p <- p + labs(x = "Time (hours)", y = "Effect")
    p
  })
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Debug/Session ---------------------------------------------------------------  
# Close the R session when browser closes
# This is nice to have, put it in the app and forget about it
	session$onSessionEnded(function(){
	  stopApp()
	})  # endsession
  
})  # shinyServer

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 