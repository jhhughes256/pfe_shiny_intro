# # Conversion from R-Script to Application -------------------------------------
# In this step, UI for changing the number of individuals simulated is finally
#   added. As a result the application becomes a bit less snappy, particularly
#   when simulating 1000 individuals! And this is re-simulated when you change
#   any of the inputs. This can be fine if users want the simulation to update
#   after a single change each time. However, if users want to change multiple
#   inputs at a time, this app could easily be classified as over-reactive.
# A checkbox is added along with server code to allow the plot to be viewed on
#   on a log-scale. Also some code to generate the tables conditioned on how 
#   many individuals are simulated. If a single individual is simulated, then 
#   calculation and display of 90% prediction intervals is not informative.
#   Therefore alternate code is provided for when NID == 1.
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