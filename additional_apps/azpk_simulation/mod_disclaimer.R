# Module UI
  
#' @title   mod_disclaimer_ui and mod_disclaimer_server
#' @description  Shiny module which handles the modal dialogue box which
#'   displays the disclaimer requiring acknowledgement prior to use of the
#'   application by the user.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_disclaimer
#'
#' @keywords internal
#' @export 
#' @import shiny

mod_disclaimer_ui <- function(id) {
# Define namespace function for IDs
  ns <- NS(id)
# Create tagList to be used in the UI
  modalDialog(
    title = fluidRow(
      div(
        icon("exclamation-circle"), 
        style = "font-size:80px; color:#F39C12"
      ),
      div(h1("ATTENTION"), align = "center"),
      align = "center"
    ),
    tagList(
      fluidRow(
        column(12,
          p(paste("The application was developed for the simulation of", 
            "azithromycin concentrations in plasma, peripheral leukocytes, lung", 
            "tissue and alveolar macrophages based on different clinical dosing", 
            "regimens of azithromycin.  Exposures can be compared to in vitro", 
            "target concentrations such as the EC90 of azithromycin determined", 
            "against SARS-CoV-2.  While this web application allows the", 
            "comparison of tissue concentrations to active in vitro targets,", 
            "efficacy still needs to be demonstrated in patients enrolled in", 
            "controlled clinical trials.")),
          p(paste("Azithromycin is not approved for the treatment of COVID-19.", 
            " No recommendations are made for the treatment of patients with", 
            "COVID-19 with azithromycin outside of an authorized clinical trial.", 
            " This application is intended for researchers to aid dosing regimen", 
            "selection of azithromycin under the controlled environment of an", 
            "authorized clinical trial.")),
          p(paste("The published model used for the simulation of azithromycin",
            "treatment regimens is documented in the reference below."), 
            tags$blockquote(paste("Hughes, J.H., Sweeney, K., Ahadieh, S. and",
              "Ouellet, D. (2020), Predictions of Systemic, Intracellular, and", 
              "Lung Concentrations of Azithromycin with Different Dosing Regimens",
              "used in COVID-19 Clinical Trials. CPT Pharmacometrics Syst. Pharmacol."), 
              a("doi:10.1002/psp4.12537", 
                href = "https://ascpt.onlinelibrary.wiley.com/doi/abs/10.1002/psp4.12537"),
              style = "font-size:14px"
          )),
          p(paste("By clicking the button below, I attest that I am a researcher",
            "and I will use this application only to aid dosing regimen selection", 
            "of azithromycin for a patient population, not for an individual", 
            "patient, in an authorized clinical trial."))
        ),
        align = "justified"
      )
    ),
    footer = div(modalButton("I acknowledge the information above"), align = "center"),
    size = "l", easyClose = FALSE
  )
}  # mod_disclaimer_ui