# Example simulation R script -------------------------------------------------
# This is for demonstration purposes and is an example of how an R script 
#   can be set up to aid conversion to a Shiny application. This script uses
#   mrgsolve for simulation, so is can be used as a foundation for simulation
#   using other mrgsolve models.
# This is best made after you've discussed the scope of the application with the
#   intended users of the application. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Prepare R Environment (non-reactive) ----------------------------------------
# This is where you should place all script dependencies.
# This includes packages, additional functions, data etc.
# Clear objects from environment
  rm(list = ls(all = TRUE))

# Load packages
  library(tidyverse)
  library(mrgsolve)
  library(PKNCA)

# Set ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 10))
  theme_update(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))

# Define summary functions
  ci90lo <- function(x) quantile(x, prob = 0.05, na.rm = TRUE)
  ci90hi <- function(x) quantile(x, prob = 0.95, na.rm = TRUE)
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Model Definition (non-reactive) ---------------------------------------------
# This is where we define the model
# In this example we are using Metrum's house mrgsolve model, a generic indirect
#   PK/PD model, with weight and sex as covariates. 
  mod <- mrgsolve:::house()
  
# Can view model code with:
  as.list(mod)$code

# Simulation Inputs (inputs) --------------------------------------------------
# These are the pieces you want users to interact with.
# Think hard about this, and include inputs you think might be desirable for
#   users to interact with. You don't have to implement everything right away,
#   and some of these inputs may never be used, but it's easier to set this up
#   now.
  NID <- 100
  DOSEAMT <- 100
  DOSEFRQ <- 24
  DOSEDUR <- 5
  COVBWT <- 70
  COVSEX <- 0
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Modify Simulation Conditions (reactive) -------------------------------------
# As this model simulates population predictions, omat() (omega matrix) and 
#   smat() (sigma matrix) are used to provide variance-covariance values for the 
#   random effects. This is only done when NID > 1. It is defined as a separate
#   object, as you should avoid changing non-reactive components of your code.
  if (NID > 1) {
    simmod <- mod %>%
      omat(diag(c(0.1, 0.1, 0.2, 0.2))) %>%
      smat(matrix(0.1))
  } else {
    simmod <- mod
  }
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Simulation Code (reactive) --------------------------------------------------
# This is the part were you provide your input to the model to generate
#   predictions.
# This code simulates from the model. param() allows the update of parameters &
#   covariates in the model. Here it is used to provide the input for the WT and
#   SEX covariates. ev() can be provided the dosing information as well as the 
#   number of individuals to be simulated. Here the dosing is defined based on
#   DOSEAMT, DOSEFRQ and DOSEDUR.
  dosetimes <- seq(0, 24*(DOSEDUR - 1), by = DOSEFRQ)
  simdf <- simmod %>%
    param(WT = COVBWT, SEX = COVSEX) %>%
    ev(amt = rep(DOSEAMT, length(dosetimes)), time = dosetimes) %>%
    carry_out(amt, evid) %>%
    mrgsim_df(nid = NID, start = 0, end = 24*(DOSEDUR + 1), delta = 0.5) %>%
    as_tibble()
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Script Outputs --------------------------------------------------------------
# This is where we create examples of types of output we want.
# Generate plot
# In this case we want a plot of the typical value (NID == 1) or median & 
#   prediction intervals (NID >= 1)
  p <- NULL
  p <- ggplot(aes(x = time, y = CP), data = simdf)
  if (NID == 1) {
    p <- p + geom_line(colour = "blue", size = 1)
  } else {
    p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
    p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
      alpha = 0.4, fill = "blue")
  }
  p <- p + labs(x = "Time (hours)", y = "Concentration (ng/mL)")
  p <- p + scale_x_continuous(breaks = 0:(DOSEDUR + 1)*24)
  p
  
# PD too!
  p <- NULL
  p <- ggplot(aes(x = time, y = RESP), data = simdf)
  if (NID == 1) {
    p <- p + geom_line(colour = "blue", size = 1)
  } else {
    p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
    p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
      alpha = 0.4, fill = "blue")
  }
  p <- p + labs(x = "Time (hours)", y = "Response")
  p <- p + scale_x_continuous(breaks = 0:(DOSEDUR + 1)*24)
  p

# Generate summary statistics for table
# Using PKNCA for NCA summary, PKNCA default is lin up/log down. Currently 
#   does not support lin/log (lin until Cmax, log after).
  pksum024 <- simdf %>%
    filter(evid == 0 & time <= DOSEFRQ) %>%
    group_by(ID) %>%
    summarise(
      AUCtau = pk.calc.auc.last(CP, time, interval = c(0, DOSEFRQ)),
      Cmax = pk.calc.cmax(CP),
      Ctrough = pk.calc.ctrough(CP, time, end = DOSEFRQ)) %>%
    mutate(Cave = pk.calc.cav(AUCtau, start = 0, end = DOSEFRQ)) %>%
    summarise_at(vars(-ID), list(Median = median, ci90lo = ci90lo, ci90hi = ci90hi)) %>%
    summarise_all(signif, digits = 3) %>%
    pivot_longer(everything()) %>%
    separate(name, c("Metric", "stat"), sep = "_") %>%
    pivot_wider(id_cols = Metric, names_from = "stat", values_from = "value") %>%
    unite("PI", ci90lo, ci90hi, sep = " - ")
  
  pksumSS <- simdf %>%
    filter(evid == 0 & time >= (24*DOSEDUR - DOSEFRQ) & time <= 24*DOSEDUR) %>%
    group_by(ID) %>%
    summarise(
      AUCtau = pk.calc.auc.last(CP, time, interval = c(24*DOSEDUR - DOSEFRQ, 24*DOSEDUR)),
      Cmax = pk.calc.cmax(CP),
      Ctrough = pk.calc.ctrough(CP, time, end = 24*DOSEDUR)) %>%
    mutate(Cave = pk.calc.cav(AUCtau, start = 24*DOSEDUR - DOSEFRQ, end = 24*DOSEDUR)) %>%
    summarise_at(vars(-ID), list(Median = median, ci90lo = ci90lo, ci90hi = ci90hi)) %>%
    summarise_all(signif, digits = 3) %>%
    pivot_longer(everything()) %>%
    separate(name, c("Metric", "stat"), sep = "_") %>%
    pivot_wider(id_cols = Metric, names_from = "stat", values_from = "value") %>%
    unite("PI", ci90lo, ci90hi, sep = " - ")
  
