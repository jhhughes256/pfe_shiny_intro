# # Conversion from R-Script to Application -------------------------------------
# In this step a button is added to the UI to allow users to control when the
#   simulation occurs. This is done using the actionButton UI and by changing
#   the server structure to include a new reactive object responsible for 
#   updating the inputs for the simulation. This is effectively recreating the
#   input object on the server, but making it such that it only updates when
#   the button is pressed. Unlike submitButton, this method gives control over
#   what should remain reactive, and what should only be updated when the button
#   is pressed.
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