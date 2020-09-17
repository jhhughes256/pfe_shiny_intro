# R script for simulating a population and concentrations as described by:
# 2-compartment model
# Dosing regimen:
#   IV bolus    (at 0 hours)
#   Oral Dosing (at 1 hours, then every 12 hours after IV bolus)
#   IV Infusion (at 72 hours, duration 10 hours)
# Population parameter variability on CL, V1, KA
# Variance-covariance matrix for CL, V1, KA
# Covariate effects:
#   Gender & Creatinine Clearance on CL

# Proportional error model (with optional additive residual)
#-------------------------------------------------------------------------------
# Define user-input dependent functions for output
shinyServer(function(input, output, session) {
# Reactive expression to generate a reactive data frame
# Whenever an input changes this function re-evaluates
  Rpar.data <- reactive({
  # Create a parameter dataframe with ID and parameter values for each individual
  # Define individual
    n <- as.numeric(input$n)  #Number of "individuals"
    ID <- seq(from = 1, to = n, by = 1)  #Simulation ID
    WT <- input$wt  #Total body weight, kg
    AGE <- input$age  #Age, years
    SECR <- input$secr  #Serum Creatinine, umol/L
    SEX <- input$sex  #Gender, Male (0) Female (1)
    SMOK <- input$smok  #Smoking Status, Not Current (0) Current (1)

  # Now use multivariate rnorm to turn the covariance matrix into ETA values
    ETAmat <- mvrnorm(n = n, mu = c(0, 0, 0), OMEGA)
    ETA1 <- ETAmat[, 1]
    ETA2 <- ETAmat[, 2]
    ETA3 <- ETAmat[, 3]

  # Define covariate effects
    SMOKCOV <- 1
    if(SMOK == 1) SMOKCOV <- SMOKCOV + COV1
    CRCL <- ((140 - AGE)*WT)/(SECR*0.815)  # Male creatinine clearance
    if(SEX == 0) CRCL <- CRCL*0.85  #Female creatinine clearance

  # Define individual parameter values
    CL <- CLPOP*exp(ETA1)*((WT/70)^0.75)*SMOKCOV*((CRCL/90)^COV2)
    V1 <- V1POP*exp(ETA2)*(WT/70)
    Q  <- QPOP*(WT/70)^0.75
    V2 <- V2POP*(WT/70)
    KA <- KAPOP*exp(ETA3)

  # Calculate rate-constants for differential equation solver
    K12 <- Q/V1
    K21 <- Q/V2
    K10 <- CL/V1

  # Collect the individual parameter values in a parameter dataframe
    par.data <- data.frame(
      ID, CL, V1, Q, V2,  #patient parameters
      KA, K12, K21, K10,  #rate constants
      WT, AGE, SECR, SEX, SMOK #covariates
    )
  })  #Rpar.data

#------------------------------------------------------------------------------
  Rsim.data <- reactive({
  # Specify oral doses
  # this uses the option to specify "events" in deSolve using a dataframe
    if (input$pocheck == FALSE) {
      oral.dose <- 0
      oral.dose.times <- 0
    } else {
      oral.dose <- input$podose
      if (input$potimes == 1) pofreq <- 24
      if (input$potimes == 2) pofreq <- 12
      if (input$potimes == 3) pofreq <- 8
      if (input$potimes == 4) pofreq <- 6
      oral.dose.times <- seq(from = input$postart, to = 120, by = pofreq)
    }

  # Define bolus dose events
  # below works for constant dosing
    oral.dose.data <- data.frame(
      var = 1,  #enters into depot compartment (A[1])
      time = oral.dose.times,
      value = oral.dose,
      method = "add"
    )

  # Specify bolus intravenous doses
  # specifies "events" as seen in oral dosing
    if (input$ivcheck == FALSE) {
      iv.dose <- 0
      iv.dose.times <- 0
    } else {
      iv.dose <- input$ivdose  #mg for first bolus dose
      iv.dose.times <- input$ivtimes  #time of first bolus (h)
    }

  # Define bolus dose events
    iv.dose.data <- data.frame(
      var = 2,  #enters into central compartment (A[2])
      time = iv.dose.times,
      value = iv.dose,
      method = "add")

  # Combine dose data
    all.dose.data <- rbind(oral.dose.data, iv.dose.data)

  # Define continuous infusion
    #this uses the approxfun function
    #makes a "forcing function" for infusion rate in the differential equations
    if (input$infcheck == FALSE) {
      inf.dose <- 0
      inf.dur <- 0
      inf.start <- 0
    } else {
      inf.dose <- input$infdose  #mg
      inf.dur <- as.numeric(input$infdur)
      inf.start <- input$inftimes
    }
    inf.rate <- inf.dose/inf.dur
    inf.times <- c(inf.start, inf.start + inf.dur)

  # Make a time sequence (hours)
    all.times <- sort(unique(c(set.time, oral.dose.times, iv.dose.times, inf.times)))
    final.time <- max(all.times)
    #The time sequence must include all "event" times for deSolve, so added here
    #Do not repeat a time so use "unique" as well

  # Calculate continuous infusion
    #if infusion starts at zero, starting 0 is not required
    #100 at end ensures the function works after 82 hours
    inf.time.data <- c(0, inf.times, final.time)
    inf.rate.data <- c(0, inf.rate, 0, 0)

#------------------------------------------------------------------------------

  # Apply simulate.conc.cmpf to each individual in par.data
  # Maintain their individual values for V1, SEX and WT for later calculations
    sim.data <- ddply(Rpar.data(), .(ID, V1), simulate.conc.cmpf,
      event.data = all.dose.data, times = all.times,
      inf.rate.fun = inf.rate.fun(inf.time.data, inf.rate.data))

  # Calculate individual concentrations in the central compartment
    sim.data$IPRED <- sim.data$A2/sim.data$V1
    return(sim.data)
  })	#Rsim.data

#----------------------------------------------------------------------------------------
#Generate a plot of the data
#Also uses the inputs to build the plot
  output$plotconc <- renderPlot({
  # Generate a plot of the sim.data
    plotobj <- NULL
    plotobj <- ggplot(data = Rsim.data())
    if (input$ci == 1) {
      plotobj <- plotobj + stat_summary(aes(x = time, y = IPRED),
        fun.ymin = CI80lo, fun.ymax = CI80hi, geom = "ribbon",
        fill = "red", alpha = 0.3)
    } else {
      plotobj <- plotobj + stat_summary(aes(x = time, y = IPRED),
        fun.ymin = CI90lo, fun.ymax = CI90hi, geom = "ribbon",
        fill = "red", alpha = 0.3)
    }
    plotobj <- plotobj + stat_summary(aes(x = time, y = IPRED),
      fun.y = median, geom = "line", size = 1, colour = "red")
    if (input$logscale == FALSE) {
      plotobj <- plotobj + scale_y_continuous("Concentration (mg/L) \n",
        breaks = seq(from = 0, to = max(Rsim.data()$IPRED), by = ceiling(max(Rsim.data()$IPRED)/10)),
        lim = c(0, max(Rsim.data()$IPRED)))
    } else {
      plotobj <- plotobj + scale_y_log10("Concentration (mg/L) \n",
        breaks = c(0.01,0.1,1,10), labels = c(0.01,0.1,1,10),
        lim = c(NA, max(Rsim.data()$IPRED)))
    }
    plotobj <- plotobj + scale_x_continuous("\nTime (hours)", lim = c(0, 120),
      breaks = seq(from = 0,to = 120,by = 24))
    print(plotobj)
  })	#renderPlot

  output$infrate <- renderText({
    inf.dur <- as.numeric(input$infdur)
    paste0("Infusion rate = ", signif(input$infdose/inf.dur, digits = 3) ," mg/hr")
  })	#renderText

  output$crcl <- renderText({
    crcl <- ((140 - input$age)*input$wt)/(input$secr*0.815)
    if (input$sex == 0) crcl <- ((140 - input$age)*input$wt)/(input$secr*0.815)*0.85
    paste0("Creatinine clearance = ", signif(crcl, digits = 3), " mL/min")
  })

  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })

})	#shinyServer
