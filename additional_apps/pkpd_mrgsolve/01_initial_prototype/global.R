# # Conversion from R-Script to Application -------------------------------------
# In this step the inputs from the original script are considered, and the UI is
#   prototyped prior to any server code being written. The non-reactive parts of
#   the original script (loading packages, datasets and subsetting) are defined
#   in the global.R script, so that they are run prior to app start-up. The UI
#   and server are separated into the ui.R and server.R scripts. This is a good
#   structure to use if you anticipate the app being developed will be large.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# global.r (Non-Reactive Server Objects) --------------------------------------
# All non-reactive components of your application are specified here. This 
#   consists of any application dependencies like your model specification,
#   packages, additional functions, data, etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Prepare R Environment (non-reactive) ----------------------------------------
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
# In this example we are using Metrum's house mrgsolve model, a generic indirect
#   PK/PD model, with weight and sex as covariates. 
  mod <- mrgsolve:::house()
  
# Can view model code with:
  # as.list(mod)$code
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 