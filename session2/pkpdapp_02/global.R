# Prepare R Environment (non-reactive) ----------------------------------------
# This is where you should place all script dependencies.
# This includes packages, additional functions, data etc.
# Clear objects from environment
  rm(list = ls(all = TRUE))

# Load packages
  library(shiny)
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
  