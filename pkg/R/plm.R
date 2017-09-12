

plm <- function(data, AR = 0, model = "B&L-B", phases = c("A","B"), family = "gaussian", formula = values ~ 1 + mt + phase + inter, na.action = na.omit, ...) {

  if (AR > 0 && !family == "gaussian")
    stop("Autoregression models could only be applied if distribution familiy = 'gaussian'.\n")
  
  data <- .SCprepareData(data, na.rm = TRUE)
  N <- length(data)
  if(N > 1)
    stop("Procedure could not be applied to more than one case.\nConsider to use the hplm function.")
  
  if(!identical(rle(as.character(data[[1]]$phase))$values, phases))
    warning(paste0("Phase sequence is not ",paste0(phases, collapse = ""), " for all cases. Analyzes are restricted to the data of the ",paste0(phases,collapse = "")," phases.\n"))

  data <- keepphasesSC(data, phases = phases)$data
  data <- data[[1]]
  
  ### model definition
  dat.inter <- .plm.interaction(data, model = model)
  
  data$mt <- dat.inter$mt
  data$inter <- dat.inter$inter

  PREDICTORS <- as.character(formula[3])
  PREDICTORS <- unlist(strsplit(PREDICTORS, "\\+"))
  PREDICTORS <- trimws(PREDICTORS)
  if(!is.na(match("1", PREDICTORS)))
     PREDICTORS <- PREDICTORS[-match("1", PREDICTORS)]
  
  formula.full <- formula
  formulas.ir <- sapply(PREDICTORS, function(x) update(formula, formula(paste0(".~. - ",x))))

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
  out <- list(model = model, F.test = F.test, r.squares = r.squares, ar = AR, family = family, full.model = full)

  class(out) <- c("sc", "pr")
  out
}


.plm.interaction <- function(data, model) {
  MT <- data$mt
  D  <- data$phase
  
  if(model == "H-M") {
    inter <- ifelse(D == "A", 0,1)
    for(i in 1:length(inter))
      if(inter[i] == 1) 
        inter[i] <- inter[i-1] + 1 
    inter <- ifelse(inter == 0,0, inter - 1) 
    
  } else if (model == "B&L-B") {
    inter <- ifelse(D == "A", 0,1)
    for(i in 1:length(inter))
      if(inter[i] > 0) 
        inter[i] <- inter[i-1] + 1 
      
  } else if (model == "Mohr#1") {
    inter <- MT * ifelse(D == "A", 0,1)
    
  } else if (model == "Mohr#2") {
    inter <- ifelse(D == "A", 0,1)
    for(i in 1:length(inter))
      if(inter[i] > 0) 
        inter[i] <- inter[i-1] + 1 
      inter <- ifelse(inter == 0,0, inter - 1) 
      n1 <- sum(data$phase == "A")
      MT <- MT-MT[n1+1] #this is correct! MT must be corrected after the calculation of the interaction term
 
  } else if (model == "Manly") {
    inter <- MT * ifelse(D == "A", 0,1)
    
  } else stop("Wrong model definition!\n")
  
  data.frame(mt = MT, inter = inter)
  
}



.plm.mt <- function(data, type = "level p", model = "B&L-B", count.data = FALSE) {
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculatioins could only be applied to a single data set.\n")
  
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
