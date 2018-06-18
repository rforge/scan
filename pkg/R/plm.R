#' Piecewise linear model / piecewise regression
#' 
#' The \code{plm} function computes a piecewise regression model (see Huitema &
#' McKean, 2000).
#' 
#' 
#' @param data A single-case data frame.
#' See \code{\link{scdf}} to learn about this format.
#' @param AR Maximal lag of autoregression. Modeled based on the
#' Autoregressive-Moving Average (ARMA) function.  When AR is set, the family
#' argument must be set to \code{family = "gaussian"}.
#' @param model Model used for computing dummy variables for the level and slope effects (see Huitema & McKean,
#' 2000). Default is \code{model = "B&L-B"}. Possible values are:
#' \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"},
#' \code{"Manly"}, \code{"JW"}, and , \code{"JW2"}.
#' @param family Set the distributioin family. Defaults to a gaussian
#' distribution. See the \code{family} function for more details.
#' @param trend A logical indicating if a trend parameters is included in the model.
#' @param level A logical indicating if a level parameters is included in the model.
#' @param slope A logical indicating if a slope parameters is included in the model.
##' @param formula Defaults to the standard piecewise regression model. The
#' parameter phase followed by the phase name (e.g., phaseB) indicates the level effect of the corresponding phase. The parameter 'inter' followed by the phase name (e.g., interB) adresses the slope effect based on the method
#' provide in the model argument (e.g., "B&L-B"). The formula can be changed
#' for example to include further variables into the regression model.
#' @param update An easier way to change the regression formula (e.g., . ~ . + newvariable).
#' @param na.action Defines how to deal with missing values
#' @param ... Further arguments passed to the glm function.
#' @return 
#' \item{formula}{plm formula. Uselful if you want to use the update or formula argument and you don't know the names of the parameters.}
#' \item{model}{Character string from function call (see \code{Arguments} above).} 
#' \item{F.test}{F-test values of modelfit.}
#' \item{r.squares}{Explained variance R squared for each model parameter.}
#' \item{ar}{Autoregression lag from function call (see \code{Arguments} above).}
#' \item{family}{Distribution family from function call (see \code{Arguments} above).}
#' \item{full.model}{Full regression model list from the gls or glm function.}
#' @author Juergen Wilbert
#' @seealso \code{\link{hplm}}, \code{\link{glm}}, \code{\link{gls}}
#' @references Beretvas, S., & Chung, H. (2008). An evaluation of modified
#' R2-change effect size indices for single-subject experimental designs.
#' \emph{Evidence-Based Communication Assessment and Intervention, 2}, 120-128.
#' 
#' Huitema, B. E., & McKean, J. W. (2000). Design specification issues in
#' time-series intervention models. \emph{Educational and Psychological
#' Measurement, 60}, 38-58.
#' @examples
#' 
#' ## Compute a piecewise regression model for a random single-case
#' set.seed(123)
#' AB <- design.rSC(n = 1, phase.design = list(A = 10, B = 20), 
#'             level = list(A = 0, B = 1), slope = list(A = 0, B = 0.05), 
#'             trend = list(0.05))
#' dat <- rSC(design = AB)
#' plm(dat, AR = 3)
#' 
#' ## Another example with a more complex design
#' 
#' set.seed(123)
#' A1B1A2B2 <- design.rSC(n = 1, rtt = 0.8,   m = 50, s = 10,
#'                   phase.design = list(A1 = 15, B1 = 20, A2 = 15, B2 = 20), 
#'                   level = list(A1 = 0, B1 = 1, A2 = -1, B2 = 1),
#'                   slope = list(A1 = 0, B1 = 0.0, A1 = 0, B2 = 0.0),
#'                   trend = list(0.0))
#' dat <- rSC(design = A1B1A2B2)
#' plm(dat, model = "JW")
#' 
#' ## no slope effects were found. Therefore you might want to drop slope estimation:
#' plm(dat, slope = FALSE, model = "JW")
#' 
#' ## and now drop the trend estimation as well
#' plm(dat, slope = FALSE, trend = FALSE, model = "JW")
#' 
#' 
#' 
#' 
#' 
plm <- function(data, AR = 0, model = "B&L-B", family = "gaussian", trend = TRUE, level = TRUE, slope = TRUE,formula = NULL, update = NULL, na.action = na.omit, ...) {

  if (AR > 0 && !family == "gaussian")
    stop("Autoregression models could only be applied if distribution familiy = 'gaussian'.\n")
  
  data <- .SCprepareData(data, na.rm = TRUE)
  ATTRIBUTES <- attributes(data)
  
  N <- length(data)
  if(N > 1)
    stop("Procedure could not be applied to more than one case.\nConsider to use the hplm function.")
  
  #if(!is.null(phases))
  #   data <- .keepphasesSC(data, phases = phases)$data
  
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
    formula <- as.formula(paste0("values ~ 1",MT, PHASE, INTER))
  } 
  
  if(!is.null(update))
    formula <- update(formula, update)
  
  PREDICTORS <- as.character(formula[3])
  PREDICTORS <- unlist(strsplit(PREDICTORS, "\\+"))
  PREDICTORS <- trimws(PREDICTORS)
  if(!is.na(match("1", PREDICTORS)))
     PREDICTORS <- PREDICTORS[-match("1", PREDICTORS)]
  
  formula.full <- formula
  formulas.ir  <- sapply(PREDICTORS, function(x) update(formula, formula(paste0(".~. - ",x))))

  if(AR == 0) {
    full <- glm(formula.full, data = data, family = family, na.action = na.action, ...)
    restricted.models <- lapply(formulas.ir, function(x) glm(x, data = data, family = family, na.action = na.action, ...))
    df2.full <- full$df.residual
    df.int <- if (attr(full$terms, "intercept")) 1 else 0
  }

  if(AR > 0) {
    full <- gls(formula.full, data = data, correlation=corARMA(p=AR), method="ML", na.action = na.action)
    restricted.models <- lapply(formulas.ir, function(x) gls(model = x, data = data, correlation=corARMA(p=AR), method="ML", na.action = na.action))
    df2.full <- full$dims$N - full$dims$p
    df.int <- if ("(Intercept)" %in% names(full$parAssign)) 1 else 0
  }
  
  n <- length(full$residuals)
  df1.full <- n-1-df2.full
  
  QSE <- sum(full$residuals^2, na.rm = TRUE)
  QST <- sum((data$values - mean(data$values))^2)
  MQSA <- (QST - QSE) / df1.full
  MQSE <- QSE / df2.full
  F.full <- MQSA / MQSE
  p.full <- pf(F.full,df1.full,df2.full, lower.tail = FALSE)
  
  
  total.variance <- var(data$values)
  r2.full <- 1-(var(full$residuals)/total.variance)
  
  r2.full.adj <- 1 - (1 - r2.full) * ((n - df.int)/df2.full)#r2.full-(1-r2.full)*(3/(n-df1.full-1))
  #r2.full.adj <- r2.full-(1-r2.full)*(3/(length(data$values)-3-1))
 
  r.squares <- unlist(lapply(restricted.models, function(x) r2.full-(1-(var(x$residuals, na.rm = TRUE)/total.variance))))
  
  ### output
  F.test <- c(F = F.full, df1 = df1.full, df2 = df2.full, p = p.full, R2 = r2.full, R2.adj = r2.full.adj)
  out <- list(formula = formula.full, model = model, F.test = F.test, r.squares = r.squares, ar = AR, family = family, full.model = full, data = data)

  class(out) <- c("sc", "pr")
  attr(out, "var.phase")  <- ATTRIBUTES$var.phase
  attr(out, "var.mt")     <- ATTRIBUTES$var.mt
  attr(out, "var.values") <- ATTRIBUTES$var.values
  out
}


.plm.dummy <- function(data, model, phase.dummy = TRUE) {

  if(!model %in% c("H-M", "B&L-B", "JW","JW2"))
    stop("Model ",model," unknown.\n")
    
  MT <- data$mt
  D  <- data$phase
  N  <- nrow(data)
  
  out    <- data.frame(mt = MT)
  design <- rle(as.character(data$phase))
  
  #dummy phases
  if(phase.dummy) {
    for(phase in 2:length(design$values)) {
      length.phase <- design$lengths[phase]
      pre <- sum(design$lengths[1:(phase-1)])
      dummy <- rep(0,N)
      
      if(model == "JW") {
        dummy[(pre + 1):N] <- 1
      } else {
        dummy[(pre + 1):(pre + length.phase)] <- 1
      }
      
      out[,paste0("phase",design$values[phase])] <- dummy
    } 
  }
  
  
  for(phase in 2:length(design$values)) {
    inter <- rep(0,N)
    length.phase <- design$lengths[phase]
    pre <- sum(design$lengths[1:(phase-1)])
    
    if(model == "B&L-B") { 
      inter[(pre +1):(pre + length.phase)] <- MT[(pre +1):(pre + length.phase)] - MT[(pre)]
    } else if (model == "H-M") {
      inter[(pre +1):(pre + length.phase)] <- MT[(pre +1):(pre + length.phase)] - MT[(pre + 1)]
    } else if (model == "JW" || model == "JW2") {
      inter[(pre +1):N] <- MT[(pre +1):N]- MT[(pre)]
    }
    
    out[,paste0("inter",design$values[phase])] <- inter
  }
  
  
  # if(model == "B&L-B") {
  #   for(phase in 2:length(design$values)) {
  #     inter <- rep(0,N)
  #     length.phase <- design$lengths[phase]
  #     pre <- sum(design$lengths[1:(phase-1)])
  #     inter[(pre +1):(pre + length.phase)] <- MT[(pre +1):(pre + length.phase)] - MT[(pre)]#1:length.phase
  #     out[,paste0("inter",design$values[phase])] <- inter
  #   }
  # }
  # if(model == "H-M") {
  #     for(phase in 2:length(design$values)) {
  #       inter <- rep(0,N)
  #       length.phase <- design$lengths[phase]
  #       pre <- sum(design$lengths[1:(phase-1)])
  #       inter[(pre +1):(pre + length.phase)] <- MT[(pre +1):(pre + length.phase)] - MT[(pre + 1)]#1:length.phase
  #       out[,paste0("inter",design$values[phase])] <- inter
  #   }
  #     
  # }
  # if(model == "JW" || model == "JW2") {
  #   for(phase in 2:length(design$values)) {
  #     inter <- rep(0,N)
  #     length.phase <- design$lengths[phase]
  #     pre <- sum(design$lengths[1:(phase-1)])
  #     inter[(pre +1):N] <- MT[(pre +1):N]- MT[(pre)]#1:length.phase
  #     out[,paste0("inter",design$values[phase])] <- inter
  #   }
  # }
  
  out
}


.plm.mt <- function(data, type = "level p", model = "B&L-B", count.data = FALSE) {
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculations could only be applied to a single data set.\n")
  
  if("list"%in%class(data))
    data <- data[[1]]
  if(ncol(data) < 3)
    data[,3] <- 1:nrow(data)
  
  y <- data[,2]
  n1 <- sum(data[,1] == "A")
  n2 <- sum(data[,1] == "B")
  
  if(model == "H-M") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D	
  } else if (model == "B&L-B") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1])*D	
  } else if (model == "Mohr#1") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D	
  } else if (model == "Mohr#2") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D
    MT <- MT-MT[n1+1]
  } else if (model == "Manly") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D
  }	
  
  if(count.data) {
    full <- glm(I(round(y)) ~ 1 + MT + D + inter, family = "poisson")
  } else full <- lm(y ~ 1 + MT + D + inter)
  
  if (type == "1" || type == "level p")
    return(summary(full)$coef[3,4])
  if (type == "2" || type == "slope p")
    return(summary(full)$coef[4,4])
  if (type == "3" || type == "level t") 
    return(summary(full)$coef[3,3])
  if (type == "4" || type == "slope t")
    return(summary(full)$coef[4,3])
  if (type == "5" || type == "level B")
    return(summary(full)$coef[3,1])
  if (type == "6" || type == "slope B")
    return(summary(full)$coef[4,1])
  if (type == "model")
    return(full)
  
}
