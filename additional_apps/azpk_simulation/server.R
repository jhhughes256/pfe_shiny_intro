#' @import shiny shinydashboard
app_server <- function(input, output, session) {
# Set ggplot2 theme
  theme_bw2 <- ggplot2::theme_set(ggplot2::theme_bw(base_size = 14, base_family = "sans"))
  ggplot2::theme_update(plot.title = ggplot2::element_text(hjust = 0.5))
  ggplot2::update_geom_defaults("text", 
    list(colour = "grey20", family = ggplot2::theme_get()$text$family))
  
# Show modal at application boot
  showModal(mod_disclaimer_ui("modal"))
  
# Compile model on server start (model is stored at session$userData[["mod"]])
  fct_compile_model(session)
  
# Call module that handles the dosing regimen input (and simulation)
  rsim <- callModule(mod_regimen_server, "reg")
  
# Call module that handles plotting simulations
  callModule(mod_plotsim_server, "poppk", rsim = rsim)
  
# Call module that handle info sub-tabs in the About tab
  callModule(mod_infotab_server, "model", fct_print_model(session$userData$mod))
  # callModule(mod_infotab_server, "session", sessionInfo())
  callModule(mod_infotab_server, "doc", includeMarkdown("documentation.md"))
  
}  # server