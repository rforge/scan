#' Hierarchical piecewise linear model / piecewise regression
#' 
#' The \code{hplm} function computes a hierarchical piecewise regression model.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param dvar Character string with the name of the dependent variable.
#' @param pvar Character string with the name of the phase variable.
#' @param mvar Character string with the name of the measurement time variable.
#' @param model Model used for calculating the slope parameter (see Huitema &
#' McKean, 2000). Default is \code{model = "B&L-B"}. Possible values are:
#' \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"}, \code{"JW"}, \code{"JW2"}, and
#' \code{"Manly"}.
#' @param method Method used to fit your model. Pass \code{"REML"} to maximize
#' the restricted log-likelihood or \code{"ML"} for maximized log-likelihood.
#' Default is \code{"ML"}.
#' @param control A list of settings for the estimation algorithm, replacing
#' the default values passed to the function \code{lmeControl} of the
#' \code{nlme} package.
#' @param random.slopes If \code{random.slopes = TRUE} random slope effects of
#' the level, trend, and treatment parameter are estimated.
#' @param lr.test If set TRUE likelihood ratio tests are calculated comparing model with vs. without random slope parameters.
#' @param ICC If \code{ICC = TRUE} an intraclass-correlation is estimated.
#' @param trend A logical indicating if a trend parameters is included in the model.
#' @param level A logical indicating if a level parameters is included in the model.
#' @param slope A logical indicating if a slope parameters is included in the model.
#' @param fixed Defaults to the fixed part of the standard piecewise regression model. The
#' parameter phase followed by the phase name (e.g., phaseB) indicates the level effect of the corresponding phase. The parameter 'inter' followed by the phase name (e.g., interB) adresses the slope effect based on the method
#' provide in the model argument (e.g., "B&L-B"). The formula can be changed
#' for example to include further L1 or L2 variables into the regression model.
#' @param random The random part of the model.
#' @param update.fixed An easier way to change the fixed model part (e.g., . ~ . + newvariable).
#' @param data.l2 A dataframe providing additional variables at Level 2. The
#' scdf File has to have names for all cases and the Level 2 dataframe has to
#' have a column named 'cases' with the names of the cases the Level 2
#' variables belong to.
#' @param ... Further arguments passed to the lme function.
#' @return 
#' \item{model}{List containing infromation about the applied model} 
#' \item{N}{Number of single-cases.}
#' \item{formla}{A list containing the fixed and the random formulas of the hplm model.}
#' \item{hplm}{Object of class lme contaning the multilevel model} 
#' \item{model.0}{Object of class lme containing the Zero Model.} 
#' \item{ICC}{List containing intraclass correlation and test parameters.}
#' \item{model.without}{Object of class gls containing the fixed effect model.}
#' @author Juergen Wilbert
#' @seealso \code{\link{plm}}
#' @examples
#' 
#' ## Compute hplm model on a MBD over fifty cases (restricted log-likelihood)
#' hplm(exampleAB_50, method = "REML", random.slopes = TRUE)
#' 
#' ## Analyzing with additional L2 variables
#' hplm(Leidig2018, data.l2 = Leidig2018_l2, 
#'      update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB, 
#'      slope = FALSE, random.slopes = TRUE, lr.test = TRUE)
#'      
#' @export

hplm <- function(data, dvar, pvar, mvar, model = "B&L-B", method = "ML", control = list(opt = "optim"), random.slopes = FALSE, lr.test = FALSE, ICC = TRUE, trend = TRUE, level = TRUE, slope = TRUE, fixed = NULL, random = NULL, update.fixed = NULL, data.l2 = NULL, ...) {

  if(missing(dvar)) 
    dvar <- attr(data, .opt$dv) 
  else 
    attr(data, .opt$dv) <- dvar

  if(missing(pvar))
    pvar <- attr(data, .opt$phase)
  else
    attr(data, .opt$phase) <- pvar
  
  if(missing(mvar))
    mvar <- attr(data, .opt$mt)
  else
    attr(data, .opt$mt) <- mvar
  
  dat <- .SCprepareData(data, change.var.values = FALSE, change.var.mt = FALSE, change.var.phase = FALSE)
  
  ATTRIBUTES <- attributes(dat)
  
  N <- length(dat)
  out <- list()
  out$model$interaction.method  <- model
  out$model$estimation.method   <- method
  out$model$lr.test             <- lr.test
  out$model$random.slopes       <- random.slopes
  out$model$ICC                 <- ICC
  out$N                         <- N
  
  if(!random.slopes && is.null(random))
    random <- as.formula("~1|case")
  
  for(case in 1: N) {
    dat_dummy <- .plm.dummy(dat[[case]], model = model, dvar = dvar, pvar = pvar, mvar = mvar)
    dat[[case]][,mvar] <- dat_dummy$mt
    dat[[case]] <- cbind(dat[[case]],dat_dummy[,-1])
    n_Var <- (ncol(dat_dummy) - 1) / 2
    VAR_INTER <- names(dat_dummy)[(ncol(dat_dummy)-n_Var+1):ncol(dat_dummy)]
    VAR_PHASE <- names(dat_dummy)[2:(n_Var+1)]
  }
  
  dat <- longSCDF(dat, l2 = data.l2)

  if(is.null(fixed)) {
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
      MT <- paste0("+ ",mvar," ")
    fixed <- formula(paste0(dvar, " ~ 1",MT, PHASE, INTER))
  } 
  
  if(!is.null(update.fixed))
    fixed <- update(fixed, update.fixed)
  
  if(is.null(random)) {
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
      MT <- paste0("+ ",mvar," ")
    random <- formula(paste0("~ 1",MT, PHASE, INTER,"|case"))
    
  }
  
  
  out$formula <- list(fixed = fixed, random = random)
  
  out$hplm <- lme(fixed = fixed, random = random, data = dat, na.action=na.omit, method = method, control=control, keep.data = FALSE, ...)
  out$hplm$call$fixed <- fixed
  if(lr.test) {
    
    PREDIC_RAND    <- unlist(strsplit(as.character(random[2]), "\\|"))[1]
    PREDIC_RAND_ID <- unlist(strsplit(as.character(random[2]), "\\|"))[2]
    PREDIC_RAND <- unlist(strsplit(PREDIC_RAND, "\\+"))
    PREDIC_RAND <- trimws(PREDIC_RAND)
    PREDIC_RAND_ID <- trimws(PREDIC_RAND_ID)

    if(length(PREDIC_RAND) == 1)
      stop("LR Test not applicable with only one random effect.")
    random.ir <- list(formula(gsub("1","-1",random)))
    for(i in 2:length(PREDIC_RAND))
      random.ir[[i]] <- formula(paste0("~",paste0(PREDIC_RAND[!PREDIC_RAND %in% PREDIC_RAND[i]], collapse = " + ")," | ",PREDIC_RAND_ID))
    
    out$random.ir$restricted <- list()
    
    for(i in 1:length(random.ir)) {
      out$random.ir$restricted[[i]] <- lme(fixed = fixed, random = random.ir[i], data = dat, na.action=na.omit, method = method, control=control, keep.data = FALSE, ...)
      out$random.ir$restricted[[i]]$call$fixed <- fixed
    }
    out$LR.test <- list()
    
    for(i in 1:length(random.ir)) {
      out$LR.test[[i]] <- anova(out$random.ir$restricted[[i]], out$hplm)
      
    }
    attr(out$random.ir, "parameters") <- c("Intercept", PREDIC_RAND)
  }
  

  if(ICC) {
    .formula.null <- as.formula(paste0(dvar," ~ 1"))
    out$model.0 <- lme(.formula.null, random =~1|case, data = dat, method = method, na.action=na.omit, control = control)
    out$model.0$call$fixed <- .formula.null
    VC <- as.numeric(VarCorr(out$model.0))
    out$ICC$value <- VC[1]/(VC[1]+VC[2])	
    out$model.without <- gls(.formula.null, data = dat, method = method, na.action=na.omit, control = control)
    out$model.without$call$model <- .formula.null
    dif <- anova(out$model.0, out$model.without)
    out$ICC$L <- dif$L.Ratio[2]
    out$ICC$p <- dif$"p-value"[2]
  } 
  
  out$model$fixed  <- fixed
  out$model$random <- random
  
  class(out) <- c("sc","hplm")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt)    <- mvar
  attr(out, .opt$dv)    <- dvar
  
  out
}

