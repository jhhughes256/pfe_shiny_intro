# Globally used objects and functions
# ------------------------------------------------------------------------------
# Load package libraries
  library(shiny)	#Interactive applications
  library(ggplot2)	#Plotting
  library(grid)	#Plotting
  library(plyr)
  library(reshape2)
  library(deSolve)	#Differential equation solver
  library(MASS)
  library(compiler)	#Compile repeatedly used functions

# Source cor2cov function (source code taken from MBESS package)
  source("cor2cov.R")

# Compile model
  source("model.R")

# ggplot2 theme for plotting
  theme_bw2 <- theme_set(theme_bw(base_size = 16))

# Function for calculating prediction intervals for plotting concentrations based on reactive input
# 80% prediction intervals
  CI80lo <- function(x) quantile(x, probs = 0.1)
  CI80hi <- function(x) quantile(x, probs = 0.9)
# 90% prediction intervals
  CI90lo <- function(x) quantile(x, probs = 0.05)
  CI90hi <- function(x) quantile(x, probs = 0.95)	

# TIME range - times where a concentration will be calculated
  set.time <- seq(from = 0, to = 120, by = 0.25)
