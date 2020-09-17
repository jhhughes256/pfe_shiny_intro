# Define parameter values
# Thetas
  CLPOP <- 10  #Clearance, L/h
  V1POP <- 50   #Volume of central compartment, L
  QPOP <-  10  #Inter-compartmental clearance, L/h
  V2POP <- 100  #Volume of peripheral compartment, L
  KAPOP <- 0.5  #Absorption rate constant, h^-1

  COV1 <- 0.5   #Effect of smoking status
  COV2 <- 1.15  #Effect of creatinine clearance on clearance

# Omegas (as SD)
  ETA1SD <- 0.16
  ETA2SD <- 0.16
  ETA3SD <- 0.16

# Specify a correlation matrix for ETA's
  R12 <- 0.5  #Correlation coefficient for CL-V1
  R13 <- 0.7  #Correlation coefficient for CL-KA
  R23 <- 0.5  #Correlation coefficient for V1-KA

# Epsilons (as SD)
  EPS1SD <- 0.3  #Proportional residual error
  EPS2SD <- 0  #Additional residual error (none for this model)

# Calculate ETA values for each subject
  cor.vec <- c(
    1, R12, R13,
    R12, 1, R23,
    R13, R23, 1)
  CORR <- matrix(cor.vec, 3, 3)

# Specify the between subject variability for CL, V1, V2
  SDVAL <- c(ETA1SD, ETA2SD, ETA3SD)

# Use this function to turn CORR and SDVAL into a covariance matrix
  OMEGA <- cor2cov(CORR, SDVAL)

  inf.rate.fun <- function(time, rate) {
    approxfun(time, rate, method = "const")
  }

# Function containing differential equations for amount in each compartment
  DES <- function(T, A, THETA, inf.rate.fun) {

    RateC <- inf.rate.fun(T)  #Infusion rate

    K12 <- THETA[1]
    K21 <- THETA[2]
    K10 <- THETA[3]
    KA <- THETA[4]

    dA <- vector(length = 3)
      dA[1] =       - KA*A[1]  #Depot - dose enters the system here
      dA[2] = RateC + KA*A[1] - K12*A[2] + K21*A[3] - K10*A[2]  #Central
      dA[3] =                   K12*A[2] - K21*A[3]  #Peripheral

    list(dA)
  }

# Compile DES function
# it's called by lsoda for each individual in the dataset
  DES.cmpf <- cmpfun(DES)

# Function for simulating concentrations for the ith patient
  simulate.conc <- function(par.data, event.data, times, inf.rate.fun) {

  # List of parameters from input for the differential equation solver
    theta.list <- c(
      "K12" = par.data$K12,
      "K21" = par.data$K21,
      "K10" = par.data$K10,
      "KA" = par.data$KA)

  # Set initial compartment conditions
    A_0 <- c(A1 = 0, A2 = 0, A3 = 0)

  # Run differential equation solver for simulated variability data
    var.data <- lsoda(A_0, times, DES.cmpf, theta.list,
      events = list(data = event.data), inf.rate.fun = inf.rate.fun)
    var.data <- as.data.frame(var.data)
  }

# Compile simulate.conc function
# it's called by ddply for each individual in the dataset
  simulate.conc.cmpf <- cmpfun(simulate.conc)