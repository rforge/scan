#' Set the variables of an scdf 
#' 
#' By default scan uses the variable names 'values'
#' as the dependend variable, 'phase' as indicator of a phase, and 'mt' as the
#' measurement-time. These variable names are attributes of an scdf. But you
#' might have other variables you want to use (e.g., another dependend
#' variable). With this function you can change these attributes of an scdf.
#' 
#' @aliases setvarSCDF
#' @param x An scdf.
#' @param var A character string specifying which variable you like to assign ("values", "phase", or "mt").
#' @param value A character string with the variable name.
#'
#' @examples
#' setvarSCDF(exampleAB_add) <- "depression"
#' describeSC(exampleAB_add)
#' setvarSCDF(exampleAB_add) <- "wellbeing"
"setvarSCDF<-" <- function(x, var = "values", value) {
  warning("This function is deprcated. Please set variables within functions (or change scdf with the attr function)")
  VALID <- c("dv","values","phase","mt")
  if(!(var %in% VALID))
    stop("Wrong variable name.\n")
  if(var == "values" || var=="dv") attr(x, .opt$dv) <- value
  if(var == "phase")  attr(x, .opt$phase)  <- value
  if(var == "mt")     attr(x, .opt$mt)     <- value
  
  x
}
