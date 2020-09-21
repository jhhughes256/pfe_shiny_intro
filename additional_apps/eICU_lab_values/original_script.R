# Explore lab data ------------------------------------------------------------
# Example application for exploring data. Lab data sourced from open-access 
#   database "eICU Collaborative Research Database Demo".
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Data reference: 
# Johnson, A., Pollard, T., Badawi, O., & Raffa, J. (2019). eICU Collaborative 
#   Research Database Demo (version 2.0). PhysioNet. 
#   https://doi.org/10.13026/gxmm-es70.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Accessed through PhysioNet:
# Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P. C., Mark, R.,
#   ... & Stanley, H. E. (2000). PhysioBank, PhysioToolkit, and PhysioNet: 
#   Components of a new research resource for complex physiologic signals. 
#   Circulation [Online]. 101 (23), pp. e215-e220.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Prepare global environment --------------------------------------------------
# Clear environment of existing R objects
  rm(list = rm(all = TRUE))

# Load package libraries
  # library(shiny)
  library(tidyverse)

# Set ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 10))
  theme_update(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))

# Load package libraries
  library(shiny)
  library(tidyverse)

# Set ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 10))
  theme_update(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))

# Load dataset
  load("eicu_labs.Rdata")  # creates loads R object `eicuData`
  
# Declare inputs for data tidying ---------------------------------------------
# Identify most common reasons for admission
  eicuData %>% 
    pull(apacheadmissiondx) %>%
    unique()
  
# Select input from this list
  admissionDx <- "Infarction, acute myocardial (MI)"
  
# Tidy dataset for exploration ------------------------------------------------
# Create subset of the data
  subsetData <- eicuData %>%
    filter(apacheadmissiondx == admissionDx)
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Declare inputs for exploratory graphics -------------------------------------
# Identify most common lab results
  subsetData %>%
    select(matches("^labresult_")) %>%
    map(function(x) sum(!is.na(x))) %>%
    magrittr::extract(. > 50) %>%
    names()
  
# Select input from this list
  plot_x <- "sodium"
  plot_y <- "potassium"
    
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Create data for exploratory graphics ----------------------------------------
  plotdf <- subsetData
  names(plotdf)[names(plotdf) == paste0("labresult_", plot_x)] <- "xaxis"
  names(plotdf)[names(plotdf) == paste0("labresult_", plot_y)] <- "yaxis"
  plotdf <- plotdf %>%
    filter(!is.na(xaxis) & !is.na(yaxis)) %>%
    mutate(xaxis = map_dbl(xaxis, mean), yaxis = map_dbl(yaxis, mean))
  xunit <- unique(unlist(plotdf[paste0("labmeasurenamesystem_", plot_x)]))
  yunit <- unique(unlist(plotdf[paste0("labmeasurenamesystem_", plot_y)]))
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Create exploratory graphics -------------------------------------------------
  p <- NULL
  p <- ggplot(plotdf)
  p <- p + geom_point(aes(x = xaxis, y = yaxis))
  p <- p + labs(x = paste0(plot_x, " (", xunit, ")"), y = paste0(plot_y, " (", yunit, ")"))
  p
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 