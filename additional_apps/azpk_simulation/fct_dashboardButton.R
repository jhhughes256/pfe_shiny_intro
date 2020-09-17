#
#' Create action button with specific colour
#' 
#' @description 
#' Creates an action button with a colour that matches shinydashboards status
#' colours
#' 
#' @param inputId The `input` slot that will be used to access the value.
#' @param label The concents of the button or link-usually a text label, but you
#' could use any other HTML, like an image.
#' @param status The shinydashboard status that will colour the button.
#' @param icon An optional `icon` to appear on the button.
#' @param width The width of the input.
#' @param style Additional style attributes to be applied to the button.
#' @param ... Named attributes to be applied to the button or link other than
#' style attributes.
#' 
#' @return An input dataset for a mrgsolve model. 
#' @keywords internal
#' @export
#' @import shiny

fct_dashboardButton <- function(inputId, label, status, icon = NULL, width = NULL, style = "", ...) {
  if (status %in% shinydashboard:::validStatuses) {
    button_colour <- purrr::when(status,
        . == "primary" ~ "#3C8DBC",
        . == "success" ~ "#00A65A",
        . == "info" ~ "#00C0EF",
        . == "warning" ~ "#F39C12",
        . == "danger" ~ "#DD4B39"
      )
  } else {
    stop("Must specify a valid status! Please check shinydashboard documentation 
      using ?validStatuses")
  }
# Create action button to be include in ui
  actionButton(
    inputId, label, icon = icon, width = width, 
    style = paste0("border-color:#FFFFFF; background-color:", button_colour,
      "; color:#FFFFFF;", style), ...
  )  # actionButton
}