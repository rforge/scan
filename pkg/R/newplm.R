

newplm <- function(data, AR = 0, model = "B&L-B", family = "gaussian",...) {
  
  data <- .SCprepareData(data)
  N <- length(data)
  
  if(N == 1)
    data <- data[[1]]
  
  if(N > 1)
    stop("Procedure could not be applied for more than one case.\nConsider to use the hplm function.")
  
  if (AR > 0 && !family == "gaussian")
		stop("Autoregression models could only be applied if distribution familiy = 'gaussian'.\n")

  data <- na.omit(data)
  
  ### model definition
  y <- data[,2]
  MT <- data[,3]
  D <- factor(data[,1])
  if(model == "H-M") {
    inter <- ifelse(substr(D,1,1) == "A", 0,1)
    for(i in 1:length(inter))
      if(inter[i] > 0) 
        inter[i] <- inter[i-1] + 1 
    inter <- ifelse(inter == 0,0, inter - 1) 
  } else if (model == "B&L-B") {
    inter <- ifelse(substr(D,1,1) == "A", 0,1)
    for(i in 1:length(inter))
      if(inter[i] > 0) 
        inter[i] <- inter[i-1] + 1 
  } else if (model == "Mohr#1") {
    inter <- MT * ifelse(substr(D,1,1) == "A", 0,1)
  } else if (model == "Mohr#2") {
    inter <- ifelse(substr(D,1,1) == "A", 0,1)
    for(i in 1:length(inter))
      if(inter[i] > 0) 
        inter[i] <- inter[i-1] + 1 
      inter <- ifelse(inter == 0,0, inter - 1) 
    n1 <- sum(data[,1] == "A")
    MT <- MT-MT[n1+1] #this is correct! MT must be corrected after the calculation of the interaction term
  } else if (model == "Manly") {
    inter <- MT * ifelse(substr(D,1,1) == "A", 0,1)
  } else stop("Wrong model definition!\n")
  
  if(AR == 0) {
    full <- glm(y ~ 1 + MT + D + inter, family = family,...)
    lr1 <- glm(y ~ 1 + MT + D, family = family,...)
    lr2 <- glm(y ~ 1 + MT + inter, family = family,...)
    lr3 <- glm(y ~ 1 + D + inter, family = family,...)
    df2.full <- full$df.residual
  }

  if(AR > 0) {
    full <- gls(y ~ 1 + MT + D + inter, correlation=corARMA(p=AR), method="ML")
    lr1 <- gls(y ~ 1 + MT + D, correlation=corARMA(p=AR), method="ML")
    lr2 <- gls(y ~ 1 + MT + inter, correlation=corARMA(p=AR), method="ML")
    lr3 <- gls(y ~ 1 + D + inter, correlation=corARMA(p=AR), method="ML")
    df2.full <- full$dims$N - full$dims$p
  }
  
  full.I <- full$coefficients[[1]]
  full.T <- full$coefficients[[2]]
  full.D <- full$coefficients[[3]]
  full.TxD <- full$coefficients[[4]]
  
  ### inference
  QSE <- sum(full$residuals^2, na.rm = TRUE)
  QST <- sum((y-mean(y))^2, na.rm = TRUE)
  MQSA <- (QST - QSE) / 3
  MQSE <- QSE / df2.full
  F.full <- MQSA / MQSE
  p.full <- pf(F.full,3,df2.full, lower.tail = FALSE)
  r2.full <- 1 - (QSE / QST)
  r2.full.adj <- r2.full-(1-r2.full)*(3/(length(y)-3-1))
  
  r2.full <- 1-(var(full$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr1 <- 1-(var(lr1$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr2 <- 1-(var(lr2$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr3 <- 1-(var(lr3$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  
  ES.slope <- r2.full-r2.lr1
  ES.level <- r2.full-r2.lr2
  ES.trend <- r2.full-r2.lr3

  ### output
  out <- list(model = model, F = F.full, df1 = 3, df2 = df2.full, p = p.full, R2 = r2.full, R2.adj = r2.full.adj, ES.slope = ES.slope, ES.level = ES.level, ES.trend = ES.trend, full.model = full, MT = MT, data = data, ar = AR, N = N, family = family)
  class(out) <- c("sc", "pr")
  out
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
