#' Multivariate Piecewise linear model / piecewise regression
#' 
#' The \code{mplm} function computes a multivariate piecewise regression model.
#' 
#' 
#' @param data A single-case data frame.
#' See \code{\link{scdf}} to learn about this format.
#' @param dvar A string or vector of strings containing the names of the dependend variables.
#' @param model Regression model used for computation (see Huitema & McKean,
#' 2000). Default is \code{model = "B&L-B"}. Possible values are:
#' \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"},
#' \code{"Manly"}, \code{"JW"}, and , \code{"JW2"}.
#' @param trend A logical indicating if a trend parameters is included in the model.
#' @param level A logical indicating if a level parameters is included in the model.
#' @param slope A logical indicating if a slope parameters is included in the model.
#' @param formula Defaults to the standard piecewise regression model. The
#' parameter phase followed by the phase name (e.g., phaseB) indicates the level effect of the corresponding phase. The parameter 'inter' followed by the phase name (e.g., interB) adresses the slope effect based on the method
#' provide in the model argument (e.g., "B&L-B"). The formula can be changed
#' for example to include further variables into the regression model.
#' @param update An easier way to change the regression formula (e.g., . ~ . + newvariable).
#' @param na.action Defines how to deal with missing values
#' @param ... Further arguments passed to the lm function.
#' @return \item{model}{Character string from function call (see
#' \code{Arguments} above).} 
#' \item{full.model}{Full regression model list}
#' @author Juergen Wilbert
#' @seealso \code{\link{plm}}
#' @examples
#' ## 
#' mplm(exampleAB_add, dvar = c("wellbeing","depression"))
#' @export

mplm <- function(data, dvar = c("values"), model = "B&L-B", trend = TRUE, level = TRUE, slope = TRUE,formula = NULL, update = NULL, na.action = na.omit, ...) {
  print(.opt$function_debugging_warning)  
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package car needed for this function to work. Please install it.",
         call. = FALSE)
    }
  
  data <- .SCprepareData(data, na.rm = TRUE, change.var.values = FALSE)
  
  N <- length(data)
  if(N > 1)
    stop("Procedure could not be applied to more than one case.\n")
  
  data <- data[[1]]
  
  ### model definition
  dat_inter <- .plm.dummy(data, model = model)
  data$mt   <- dat_inter$mt
  data      <- cbind(data,dat_inter[,-1])
  n_Var     <- (ncol(dat_inter) - 1) / 2
  VAR_INTER <- names(dat_inter)[(ncol(dat_inter)-n_Var+1):ncol(dat_inter)]
  VAR_PHASE <- names(dat_inter)[2:(n_Var+1)]
  
  if(is.null(formula)) {
    INTER <- ""
    PHASE <- ""
    MT    <- ""
    if(slope) {
      INTER <- paste0(VAR_INTER, collapse = "+")
      INTER <- paste0("+ ", INTER)
    }
    if(level) {
      PHASE <- paste0(VAR_PHASE, collapse = "+")
      PHASE <- paste0("+ ", PHASE)
    }
    if(trend)
      MT <- "+ mt "
    formula <- as.formula(paste0("y ~ 1",MT, PHASE, INTER))
  } 
  
  if(!is.null(update))
    formula <- update(formula, update)
  
  PREDICTORS <- as.character(formula[3])
  PREDICTORS <- unlist(strsplit(PREDICTORS, "\\+"))
  PREDICTORS <- trimws(PREDICTORS)
  if(!is.na(match("1", PREDICTORS)))
     PREDICTORS <- PREDICTORS[-match("1", PREDICTORS)]
  
  formula.full <- formula
 
  y <- as.matrix(data[,dvar])

  full <- lm(formula.full, data = data,  na.action = na.action,...)
  out <- list(model = model, full.model = full)
  
  class(out) <- c("sc", "mpr")
  out
}
