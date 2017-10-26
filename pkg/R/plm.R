
plm <- function(data, AR = 0, model = "B&L-B", phases = NULL, family = "gaussian", formula = NULL, na.action = na.omit, ...) {

  if (AR > 0 && !family == "gaussian")
    stop("Autoregression models could only be applied if distribution familiy = 'gaussian'.\n")
  
  data <- .SCprepareData(data, na.rm = TRUE)
  N <- length(data)
  if(N > 1)
    stop("Procedure could not be applied to more than one case.\nConsider to use the hplm function.")
  
  if(!is.null(phases))
     data <- keepphasesSC(data, phases = phases)$data
  data <- data[[1]]
  
  ### model definition
  dat_inter <- .plm.predictor(data, model = model)
  data$mt   <- dat_inter$mt
  data      <- cbind(data,dat_inter[,-1])
  n_Var     <- (ncol(dat_inter) - 1) / 2
  VAR_INTER <- names(dat_inter)[(ncol(dat_inter)-n_Var+1):ncol(dat_inter)]
  VAR_PHASE <- names(dat_inter)[2:(n_Var+1)]
  
  if(is.null(formula)) {
    INTER <- paste0(VAR_INTER, collapse = "+")
    PHASE <- paste0(VAR_PHASE, collapse = "+")
    formula <- as.formula(paste0("values ~ 1 + mt + ", PHASE, "+", INTER))
  } 
  
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


.plm.predictor <- function(data, model, phase.dummy = TRUE) {

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
      dummy[(pre + 1):(pre + length.phase)] <- 1
      out[,paste0("phase",design$values[phase])] <- dummy
    } 
  }
  
  if(model == "B&L-B") {
    for(phase in 2:length(design$values)) {
      inter <- rep(0,N)
      length.phase <- design$lengths[phase]
      pre <- sum(design$lengths[1:(phase-1)])
      inter[(pre +1):(pre + length.phase)] <- MT[(pre +1):(pre + length.phase)] - MT[(pre)]#1:length.phase
      out[,paste0("inter",design$values[phase])] <- inter
    }
  }
  if(model == "H-M") {
      for(phase in 2:length(design$values)) {
        inter <- rep(0,N)
        length.phase <- design$lengths[phase]
        pre <- sum(design$lengths[1:(phase-1)])
        inter[(pre +1):(pre + length.phase)] <- MT[(pre +1):(pre + length.phase)] - MT[(pre + 1)]#1:length.phase
        out[,paste0("inter",design$values[phase])] <- inter
    }
      
  }
  out
}


.plm.mt <- function(data, type = "level p", model = "B&L-B", count.data = FALSE) {
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculations could only be applied to a single data set.\n")
  
  if(class(data)=="list")
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
