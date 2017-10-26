hplm <- function(data, model = "B&L-B", method = "ML", control = list(opt = "optim"), random.slopes = FALSE, lr.test = FALSE, ICC = TRUE, fixed = NULL, random = NULL, data.l2 = NULL) {
  
  formula <- fixed
  
  if(!random.slopes && is.null(random))
    random <- as.formula("~1|case")
  
  dat <- .SCprepareData(data)
  
  N <- length(dat)
  out <- list()
  out$model$interaction.method  <- model
  out$model$estimation.method   <- method
  out$model$lr.test             <- lr.test
  out$model$random.slopes       <- random.slopes
  out$model$ICC                 <- ICC
  out$N                         <- N
  
  for(case in 1: N) {
    dat_dummy <- .plm.predictor(dat[[case]], model = model)
    dat[[case]]$mt <- dat_dummy$mt
    dat[[case]] <- cbind(dat[[case]],dat_dummy[,-1])
    n_Var <- (ncol(dat_dummy) - 1) / 2
    VAR_INTER <- names(dat_dummy)[(ncol(dat_dummy)-n_Var+1):ncol(dat_dummy)]
    VAR_PHASE <- names(dat_dummy)[2:(n_Var+1)]
  }
  
  dat <- longSCDF(dat, l2 = data.l2)

  if(is.null(formula)) {
    INTER <- paste0(VAR_INTER, collapse = "+")
    PHASE <- paste0(VAR_PHASE, collapse = "+")
    formula <- as.formula(paste0("values ~ 1 + mt + ", PHASE, "+", INTER))
  } 
  
  if(is.null(random)) {
    INTER <- paste0(VAR_INTER, collapse = "+")
    PHASE <- paste0(VAR_PHASE, collapse = "+")
    random <- as.formula(paste0("~ 1 + mt + ", PHASE, "+", INTER,"|case"))
  }
  
  formula.fixed <- formula
  out$hplm <- lme(formula.fixed, random = random, data = dat, na.action=na.omit, method = method, control=control, keep.data = FALSE)

  if(lr.test) {
    
    PREDIC_RAND    <- unlist(strsplit(as.character(random[2]), "\\|"))[1]
    PREDIC_RAND_ID <- unlist(strsplit(as.character(random[2]), "\\|"))[2]
    PREDIC_RAND <- unlist(strsplit(PREDIC_RAND, "\\+"))
    PREDIC_RAND <- trimws(PREDIC_RAND)
    PREDIC_RAND_ID <- trimws(PREDIC_RAND_ID)
    if(!is.na(match("1", PREDIC_RAND)))
      PREDIC_RAND <- PREDIC_RAND[-match("1", PREDIC_RAND)]
    
    random.ir <- list(formula(gsub("1","-1",random)))
    for(i in 1:length(PREDIC_RAND))
      random.ir[[i+1]] <- formula(paste0("~ 1 + ",paste0(PREDIC_RAND[!PREDIC_RAND %in% PREDIC_RAND[i]], collapse = " + ")," | ",PREDIC_RAND_ID))
    
    out$random.ir$restricted <- list()
    for(i in 1:length(random.ir))
      out$random.ir$restricted[[i]] <- lme(formula.fixed, random = random.ir[i], data = dat, na.action=na.omit, method = method, control=control, keep.data = FALSE)
    
    out$LR.test <- list()
    for(i in 1:length(random.ir))
      out$LR.test[[i]] <- anova(out$random.ir$restricted[[i]], out$hplm)
    
    attr(out$random.ir, "parameters") <- c("Intercept", PREDIC_RAND)
  }
  

  if(ICC) {
    out$model.0 <- lme(values ~ 1, random =~1|case, data = dat, method = method, na.action=na.omit, control = control)
    VC <- as.numeric(VarCorr(out$model.0))
    out$ICC$value <- VC[1]/(VC[1]+VC[2])	
    out$model.without <- gls(values ~ 1, data = dat, method = method, na.action=na.omit, control = control)
    dif <- anova(out$model.0, out$model.without)
    out$ICC$L <- dif$L.Ratio[2]
    out$ICC$p <- dif$"p-value"[2]
  } 
  
  out$model$fixed  <- formula
  out$model$random <- random
  
  
  class(out) <- c("sc","hplm")
  
  out
}

