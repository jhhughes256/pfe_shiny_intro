# Example simulation R script -------------------------------------------------
# This is for demonstration purposes and is an example of how an R script 
#   can be set up to aid conversion to a Shiny application. Don't use this for
#   an example of what outputs you want in a Shiny app, the outputs used here
#   are very rough. This is more to think about the structure in relation to
#   a Shiny app rather than the content itself.
# This is best made after you've discussed the scope of the application with the
#   intended users of the application. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Prepare R Environment -------------------------------------------------------
# This is where you should place all script dependencies.
# This includes packages, additional functions, data etc.
# Clear objects from environment
  rm(list = ls(all = TRUE))

# Load packages
  library(ggplot2)

# Define summary functions
  ci90lo <- function(x) quantile(x, prob = 0.05, na.rm = TRUE)
  ci90hi <- function(x) quantile(x, prob = 0.95, na.rm = TRUE)

# Simulation Inputs -----------------------------------------------------------
# These are the pieces you want users to interact with.
# Think hard about this, and include inputs you think might be desirable for
#   users to interact with. You don't have to implement everything right away,
#   and some of these inputs may never be used, but it's easier to set this up
#   now.
  NID <- 10
  DOSEAMT <- 100
  DOSEFRQ <- 24
  DOSEDUR <- 5
  FOOD <- 1
  CRCL <- 70
  NTIMEPOINTS <- 60
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Model Definition ------------------------------------------------------------
# This is where we define the model
# In this example we are using analytical solutions. If using `mrgsolve` or
#   `RxODE` pasting or sourcing model code here is recommended.
# Thetas
# PK
  POPCL <- 10  # Clearance, L/h
  POPV1 <- 50   # Volume of central compartment, L
  POPQ <- 10  # Inter-compartmental clearance, L/h
  POPV2 <- 100  # Volume of peripheral compartment, L
  POPKA <- 2  #A bsorption rate constant, h^-1
  COVFOOD <- -0.5  # Effect of smoking status
  COVCRCL <- 1.15  # Effect of creatinine clearance on clearance
  
# PD
  POPEMAX <- 0.5
  POPEC50 <- 0.4

# Omegas (as variances)
  PPVCL <- 0.0256
  PPVV2 <- 0.0256
  PPVKA <- 0.0256

# Covariances (full omega block)
  R12 <- 0.0128  # Correlation coefficient for CL-V2
  R13 <- 0.01792  # Correlation coefficient for CL-KA
  R23 <- 0.0128  # Correlation coefficient for V2-KA

# Create variance-covariance matrix
  varcov.vec <- c(
    PPVCL,   R12,   R13,
      R12, PPVV2,   R23,
      R13,   R23, PPVKA)
  VARCOV <- matrix(varcov.vec, 3, 3)

# Epsilons (as SD)
  EPS1SD <- 0.3  # Proportional residual error
  EPS2SD <- 0  # Additional residual error (none for this model)
  
# Model functions
  twocompabs_fn <- function(par, TIME) {
    k10 <- par$CL/par$V1
    k12 <- par$Q/par$V1
    k21 <- par$Q/par$V2
    apb <- k10 + k12 + k21
    amb <- k10*k21
    alpha <- (apb + sqrt(apb^2 - 4*amb))/2
    beta <- (apb - sqrt(apb^2 - 4*amb))/2
    A <- par$KA*(k21 - alpha)/(par$V1*(par$KA - alpha)*(beta - alpha))
    B <- par$KA*(k21 - beta)/(par$V1*(par$KA - beta)*(alpha - beta))
    amt <- par$dose*par$F1
    data.frame(
      TIME = TIME,
      CONC = amt*(A*exp(-alpha*TIME) + B*exp(-beta*TIME) - (A+B)*exp(-par$KA*TIME))
    )
  }
  
  emax_fn <- function(par, CONC) {
    RESP <- 1 - par$EMAX*CONC/(par$EC50 + CONC)
  }
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Simulation Code -------------------------------------------------------------
# This is the part were you provide your input to the model to generate
#   predictions.
# Generate parameters for model
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
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Script Outputs --------------------------------------------------------------
# This is where we create examples of types of output we want.
# Generate plot
# In this case we want a plot of the typical value (NID == 1) or median & 
#   prediction intervals (NID >= 1)
  p <- NULL
  p <- ggplot(aes(x = TIME, y = CONC), data = simdf)
  if (NID == 1) {
    p <- p + geom_line(colour = "blue", size = 1)
  } else {
    p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
    p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
      alpha = 0.4, fill = "blue")
  }
  p
  
# PD too!
  p <- NULL
  p <- ggplot(aes(x = TIME, y = RESP), data = simdf)
  if (NID == 1) {
    p <- p + geom_line(colour = "blue", size = 1)
  } else {
    p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
    p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
      alpha = 0.4, fill = "blue")
  }
  p

# Generate summary statistics for table
# Careful using this functions for any real work, cmax is not necessarily at
#   steady-state, AUC uses linear trapezoidal method.
  cmax024 <- vapply(simlst, FUN.VALUE = double(1), FUN = function(df) {
    subdf <- df[df$TIME <= DOSEFRQ, ]
    max(subdf$CONC)
  })
  cmaxss <- vapply(simlst, FUN.VALUE = double(1), FUN = function(df) max(df$CONC))
  auc024 <- vapply(simlst, FUN.VALUE = double(1), FUN = function(df) {
    subdf <- df[df$TIME <= DOSEFRQ, ]
    C1C2 <- head(subdf$CONC, -1) + tail(subdf$CONC, -1)
    dT <- diff(subdf$TIME)
    sum(0.5*C1C2*dT)
  })
  aucss <- vapply(simlst, FUN.VALUE = double(1), FUN = function(df) {
    subdf <- df[df$TIME >= (DOSEFRQ*NDOSE - DOSEFRQ), ]
    C1C2 <- head(subdf$CONC, -1) + tail(subdf$CONC, -1)
    dT <- diff(subdf$TIME)
    sum(0.5*C1C2*dT)
  })
  
# Generate table
  if (NID == 1) {
    summdf <- tibble::tibble(
      Metric = c(
        "Cmax (single dose)", 
        "Cmax (steady-state)", 
        "AUC (0-tau)", 
        "AUC (steady-state)"),
      Value = signif(c(cmax024, cmaxss, auc024, aucss), 3)
    )
  } else {
    summdf <- tibble::tibble(
      Metric = c(
        "Cmax (single dose)", 
        "Cmax (steady-state)", 
        "AUC (0-tau)", 
        "AUC (steady-state)"),
      Median = signif(c(median(cmax024), median(cmaxss), median(auc024), median(aucss)), 3),
      `90% PI` = c(
        paste(signif(ci90lo(cmax024), 3), "-", signif(ci90hi(cmax024), 3)),
        paste(signif(ci90lo(auc024), 3), "-", signif(ci90hi(auc024), 3)),
        paste(signif(ci90lo(cmaxss), 3), "-", signif(ci90hi(cmaxss), 3)),
        paste(signif(ci90lo(aucss), 3), "-", signif(ci90hi(aucss), 3))
      )
    )
  }

  