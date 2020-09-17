# global.r (Non-Reactive Server Objects) --------------------------------------
# All non-reactive components of your application are specified here. This 
#   consists of any application dependencies like your model specification,
#   packages, additional functions, data, etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Set up R environment prior to initiating application ------------------------
# Load packages
  library(shiny)
  library(ggplot2)

# Define summary functions
  ci90lo <- function(x) quantile(x, prob = 0.05, na.rm = TRUE)
  ci90hi <- function(x) quantile(x, prob = 0.95, na.rm = TRUE)
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Model Definition ------------------------------------------------------------
# This is where we define the model
# In this example we are using analytical solutions. If using `mrgsolve` or
#   `RxODE` pasting or sourcing model code here is recommended.
# Thetas
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