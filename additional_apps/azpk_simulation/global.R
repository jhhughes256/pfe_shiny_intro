# Import packages and utility functions ---------------------------------------
  library(shiny)
  library(shinydashboard)
  `%>%` <- magrittr::`%>%`

# Source in Shiny modules -----------------------------------------------------
  source("mod_disclaimer.R")
  source("mod_regimen.R")
  source("mod_simulate.R")
  source("mod_plotsim.R")
  source("mod_tablesim.R")
  source("mod_infotab.R")

# Source in business logic ----------------------------------------------------
# Utility
  source("fct_dashboardButton.R")
  source("fct_plot_summary.R")
# Model
  source("fct_compile_model.R")
  source("fct_print_model.R")
  source("fct_simulate_model.R")
# Processes
  source("fct_process_plotlines.R")
  source("fct_process_plotribbons.R")
  source("fct_process_table.R")
  source("fct_process_regimentext.R")

# Set PATH for mrgsolve and run app -------------------------------------------
  if(interactive()) {
  # Get version of R
    Rversion <- paste("R-", getRversion(), sep="")
  # Set path for Rtools
    Sys.setenv(PATH = paste0(glue::glue(
      "c:/program files/R/{Rversion}/bin/x64/;", "c:/RTools/bin/;",
      "c:/RTools/mingw_64/bin/;", Sys.getenv("PATH")
    )))  # Sys.setenv
  }
