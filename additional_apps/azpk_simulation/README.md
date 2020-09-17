# Azithromycin Pharmacokinetic Simulations

This repository provides code for the web application described in the manuscript:

Hughes, J.H., Sweeney, K., Ahadieh, S. and Ouellet, D. (2020), Predictions of Systemic, Intracellular, and Lung Concentrations of Azithromycin with Different Dosing Regimens used in COVID‐19 Clinical Trials. CPT Pharmacometrics Syst. Pharmacol. [doi:10.1002/psp4.12537](https://ascpt.onlinelibrary.wiley.com/doi/abs/10.1002/psp4.12537)

## Purpose of the Application

The application was developed for the simulation of azithromycin treatment regimens and comparison of model
predictions in plasma, peripheral leukocytes, lung and alveolar macrophages to effective concentratrions of azithromycin against COVID-19. 

This application is intended to aid dosing regimen selection for azithromycin in the treatment of COVID-19 conducted under the controlled environment of a registered clinical trial.

To run the web application locally, clone or download the repository to your computer. Open the `global.R` file using RStudio and click the Run button located at the top-centre of the window. The application uses the inputs in the left box as input to produce the outputs in the right box. Outputs come from simulation of the population pharmacokinetic model. 

### Disclaimer

Azithromycin is not approved by the FDA for treatment of COVID-19. No recommendations are made for the treatment of COVID-19 with azithromycin outside of a registered clinical trial.

While this web application allows the comparison of in-vitro effective concentration values with tissue exposure, these values may not be indicative of effective concentrations in patients with COVID-19.

## Manuscript Code

This repository also includes code used to produce the analyses and figures in the manuscript. This code describes:

- the data preparation for digitised data used in the manuscript
- the comparison of different models from literature and extension of these existing models
- the simulations conducted using the final model
- the sensitivity analyses conducted on the final model

This code may require some changes to file names and directories to be run locally.
