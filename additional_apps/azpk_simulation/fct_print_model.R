#
#' Print `mrgsolve` model info
#' 
#' @description 
#' Prints info about the `mrgsolve` model. Works independently 
#' 
#' @param mod mrgsolve model
#' 
#' @return Model information from mrgsolve compile model object
#' @keywords internal
#' @export
#' 
fct_print_model <- function(mod) {
  mrgsolve::blocks(mod, MAIN, ODE, TABLE)
  print("$PARAM")
  print(mrgsolve::param(mod))
  print("$OMEGA")
  print(mrgsolve::omat(mod))
  print("$SIGMA")
  print(mrgsolve::smat(mod))
}