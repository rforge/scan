#' Piecewise linear model / piecewise regression
#' 
#' The \code{plm} function computes a piecewise regression model (see Huitema &
#' McKean, 2000).
#' 
#' 
#' @param data A single-case data frame or a list of single-case data frames.
#' See \code{\link{makeSCDF}} to learn about this format.
#' @param AR Maximal lag of autoregression. Modeled based on the
#' Autoregressive-Moving Average (ARMA) function.  When AR is set, the family
#' argument must be set to \code{family = "gaussian"}.
#' @param model Regression model used for computation (see Huitema & McKean,
#' 2000). Default is \code{model = "B&L-B"}. Possible values are:
#' \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"},
#' \code{"Manly"}, \code{"JW"}, and , \code{"JW2"}.
#' @param family Set the distributioin family. Defaults to a gaussian
#' distribution. See the \code{family} function for more details.
#' @param phases -
#' @param formula -
#' @param na.action Defines how to deal with missing values
#' @param ... Further arguments passed to the glm function.
#' @return \item{model}{Character string from function call (see
#' \code{Arguments} above).} \item{F}{F value for specified model.}
#' \item{df1}{Degrees of freedom (Regression).} \item{df2}{Degrees of freedom
#' (Residual).} \item{p}{P value for specified model.} \item{R2}{Explained
#' variance R squared.} \item{R2.adj}{Adjusted R squared.}
#' \item{count.data}{Logical argument from function call (see \code{Arguments}
#' above).} \item{ES.slope}{Effect size / Explained variance gain of slope.}
#' \item{ES.trend}{Effect size / Explained variance gain of trend.}
#' \item{full.model}{Full regression model list (including \code{coefficients},
#' \code{residuals} and many others} \item{MT}{Number of measurements.}
#' \item{data}{Single-case data frame passed to the function.} \item{N}{Number
#' of single-cases.} \item{family}{Character string from function call (see
#' \code{Arguments} above).}
#' @author Juergen Wilbert
#' @seealso \code{\link{hplm}}
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
plm <- function(data, AR = 0, model = "B&L-B", phases = NULL, family = "gaussian", trend = TRUE, level = TRUE, slope = TRUE,formula = NULL, update = NULL, na.action = na.omit, ...) {

  if (AR > 0 && !family == "gaussian")
    stop("Autoregression models could only be applied if distribution familiy = 'gaussian'.\n")
  
  data <- .SCprepareData(data, na.rm = TRUE)
  N <- length(data)
  if(N > 1)
    stop("Procedure could not be applied to more than one case.\nConsider to use the hplm function.")
  
  if(!is.null(phases))
     data <- .keepphasesSC(data, phases = phases)$data
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
  }

  if(AR > 0) {
    full <- gls(formula.full, data = data, correlation=corARMA(p=AR), method="ML", na.action = na.action)
    restricted.models <- lapply(formulas.ir, function(x) gls(model = x, data = data, correlation=corARMA(p=AR), method="ML", na.action = na.action))
    df2.full <- full$dims$N - full$dims$p
  }
  
  ### inference
  QSE <- sum(full$residuals^2, na.rm = TRUE)
  QST <- sum((data$values - mean(data$values))^2)
  MQSA <- (QST - QSE) / 3
  MQSE <- QSE / df2.full
  F.full <- MQSA / MQSE
  p.full <- pf(F.full,3,df2.full, lower.tail = FALSE)
  
  #r2.full <- 1 - (QSE / QST)
  
  total.variance <- var(data$values)
  r2.full <- 1-(var(full$residuals)/total.variance)
  r2.full.adj <- r2.full-(1-r2.full)*(3/(length(data$values)-3-1))
 
  r.squares <- unlist(lapply(restricted.models, function(x) r2.full-(1-(var(x$residuals, na.rm = TRUE)/total.variance))))
  
  ### output
  F.test <- c(F = F.full, df1 = 3, df2 = df2.full, p = p.full, R2 = r2.full, R2.adj = r2.full.adj)
  out <- list(model = model, F.test = F.test, r.squares = r.squares, ar = AR, family = family, full.model = full, data = data)

  class(out) <- c("sc", "pr")
  out
}


.plm.dummy <- function(data, model, phase.dummy = TRUE) {

  if(!model %in% c("H-M", "B&L-B", "JW","JW2"))
    stop("Model ",model," unknown.\n")
    
  MT <- data$mt
  D  <- data$phase
  N  <- length(D)
  
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
