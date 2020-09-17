# Azithromycin Pharmacokinetic Simulation Shiny Application
## Shiny Application Feature List
### Simulation Tab

The application uses the inputs in the left box as input to produce the outputs 
in the right box. Outputs come from simulation of the population pharmacokinetic
model described in the manuscript:

*Hughes, J.H., Sweeney, K., Ahadieh, S. and Ouellet, D. (2020), Predictions of Systemic, Intracellular, and Lung Concentrations of Azithromycin with Different Dosing Regimens used in COVID‐19 Clinical Trials. CPT Pharmacometrics Syst. Pharmacol. [doi:10.1002/psp4.12537](https://ascpt.onlinelibrary.wiley.com/doi/abs/10.1002/psp4.12537)*

* Update Dosing Regimen button begins the simulation using the current inputs
    + A simulation occurs using the default values on starting the application
    + Simulated values are displayed in the figure and table in the right box
    + Dosing regimen will be checked and corrected for errors prior to simulation
* Save Current Output button saves the current simulation, so that it can be 
  compared with simulations made with different inputs
    + Saved output is presented in the figure and table in the right box
* Clear Saved Output buttons clears the saved simulation
    + This removes saved output from figure and table in the right box
    
#### Simulation Inputs

* Population Size for Simulation, Mean Body Weight and Simulation
  Duration work as standard inputs for the simulation
* Dosing Regimen and In Vitro EC50/EC90 work as standard inputs for the 
  simulation, but also allow for user-defined input
* Dosing Regimen Input allows for user-defined input
    + Select "User-Defined" from drop-down box
    + Type number values into dose amount, interval and duration boxes to adjust
      the simulated dosing regimen
    + Press "Add" and "Remove" to add and remove additional rows for describing
      the custom dosing regimen (maximum number of rows is 3)
    + If dosing regimen has an error then a warning will be shown, and the error
      will be corrected prior to simulation.
        - When dosing interval is larger than dosing duration, dosing duration 
          is changed to match the dosing interval
        - If dosing interval doesn't divide evenly into dosing duration, the 
          remainder from dividing the dosing duration by the dosing interval is 
          subtracted from the dosing interval
* In Vitro EC50 and EC90 allows for user-defined input
    + Select "User-Defined" from drop-down box
    + Type number values into EC50 and EC90 boxes to adjust values to match
      updated EC values for SARS-CoV-2 or EC values for other microbials of
      interest
    
#### Simulation Outputs

* The figure represents the model predictions in alveolar macrophage, lung,
  plasma and white blood cell compartments of the simulated model, with an 
  overlay of the in-vitro EC50 and EC90.
    + When saved simulation output exists, those predictions are displayed in 
      addition to the current simulation
        - Coloured solid lines and shaded areas represent the current simulation
        - Coloured dashed lines and dotted lines represent the saved simulation
    + When a different EC50 or EC90 was used for the saved simulation, a black
      dashed line with reduced opacity is used to represent the saved EC value
    + When Number of Individuals for Simulation is equal to 1 these predictions
      represent the population typical prediction based on the model
    + When Number of Individuals for Simulation is more than 1 these predictions
      represent the median prediction (coloured solid/dashed line) and the 
      prediction intervals (shaded area/dotted lines)
        - The shaded area represents the 20, 40, 60, 80 and 90 percent 
          prediction intervals of the current simulation
        - The dotted line represents the 90 percent prediction intervals of the
          saved simulation
* The text above each table represents the dosing regimen used for the current
  (left) and saved (right) simulation output.
* The table presents the inputs for the current (left) and saved (right) 
  simulation output
    + The time above EC values for both alveolar macrophages and lung are 
      determined during model simulation
    + When Number of Individuals for Simulation is equal to 1 the time above EC 
      values represent the population typical values
    + When Number of Individuals for Simulation is more than 1 the time above EC 
      values represent the median and 90 percent prediction intervals
    + All time above EC values are rounded to a single decimal place

### Model Tab

Displays `mrgsolve` model blocks for:

* \$MAIN
    + Describes the calculation of base and additional model parameters using
      the model estimates
    + Equivalent to \$PK in NONMEM
* \$ODE 
    + Describes the computation of ordinary differential equations
    + Equivalent to \$DES in NONMEM
* \$TABLE 
    + Describes the conversion of differential equation output to predictions  
      and the addition of intra-individual error
    + Equivalent to \$ERROR in NONMEM
* \$PARAM 
    + Describes the model estimates
    + Equivalent to \$THETA in NONMEM
* \$OMEGA
    + Describes the full matrix of variances and covariances used to model the
      inter-individual variability
    + Equivalent to \$OMEGA in NONMEM
* \$SIGMA
    + Describes the full matrix of variances and covariances used to model the
      intra-individual variability
    + Equivalent to \$SIGMA in NONMEM

### Resource Tab

* Displays R session info providing information on:
    + R version
    + Loaded R packages
    + R package versions

## RStudio Connect GxP Summary

### Version Information
__Author/Maintainer:__ Jim H. Hughes

__Version:__ 1.0.1

__Revisions:__

* v0.0.9 17 April 2020 - Prototype
* v0.9.8 23 April 2020 - Pre-QC version
* v0.9.9 24 April 2020 - First round of QC
* v1.0.0 27 May 2020 - GitHub release
* v1.0.1 16 June 2020 - pfizer.com release

### Intended scope of use
For simulation of azithromycin treatment regimens and comparison of model
predictions to effective concentratrions of azithromycin by readers of the
manuscript:

*Hughes, J.H., Sweeney, K., Ahadieh, S. and Ouellet, D. (2020), Predictions of Systemic, Intracellular, and Lung Concentrations of Azithromycin with Different Dosing Regimens used in COVID‐19 Clinical Trials. CPT Pharmacometrics Syst. Pharmacol. [doi:10.1002/psp4.12537](https://ascpt.onlinelibrary.wiley.com/doi/abs/10.1002/psp4.12537)*

### Source Code
Source code is available from the following sources:

* Public GitHub Repository
    + GitHub / jhhughes256 / azithro_pk / master
    + https://github.com/jhhughes256/azithro_pk/tree/master
* Pfizer Internal Repository
    + improve artifact ID: CP1:ST-4348083

### Pre-requisites
This code runs on R version 3.6.1 and requires the following packages in 
addition to shiny (v1.3.2):

* `shinydashboard` - v0.7.1
* `shinyWidgets` - v0.4.9
* `tidyverse` - v1.2.1
* `mrgsolve` - v0.9.2

### Compliance with Pfizer policies
No personally identifiable data is used within this application.
All packages used in this application are open source and publically
available on CRAN or GitHub.

### IP/Document classification:
Content within the application is not considered to be sensitive given
that all associated source code and R packages are open to the public.
Access to this application on RStudio Connect should be "Anyone - no login
required".



