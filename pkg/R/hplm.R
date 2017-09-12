

hplm <- function(data, model = "B&L-B", method = "ML", control = list(opt = "optim"), random.slopes = FALSE, ICC = TRUE, phases = c("A","B"), formula = values ~ 1 + mt + phase + inter, data.l2 = NULL) {
  
  if(!is.null(data.l2) && random.slopes)
    stop("L2 model defintion and calculation only available for random intercept models.")
  dat <- .SCprepareData(data)
  
  if(!all(unlist(lapply(dat, function(x) identical(rle(as.character(x$phase))$values, phases)))))
    warning(paste0("Phase sequence is not ",paste0(phases, collapse = " "), " for all cases. Analyzes are restricted to the data of the ",paste0(phases,collapse = "")," phases.\n"))
  dat <- keepphasesSC(dat, phases = phases)$data
  
  N <- length(dat)
  
  dat <- longSCDF(dat, l2 = data.l2, model = model)
  dat$phase <- ifelse(dat$phase == "A",0,1)
  out <- list()
  out$model <- model
  out$method <- method
  out$N <- N
  out$analyze.random.slopes <- random.slopes
  out$analyze.ICC <- ICC
  
  if(!random.slopes) {
    out$random.intercept$model <- lme(formula, random =~1|case, data = dat, na.action=na.omit, method = method, control=control)
  }
  if(random.slopes) {
    out$random.trend.level$model <- lme(values ~ 1 + mt + phase + inter, random =~ 1 + mt + phase |case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend.slope$model <- lme(values ~ 1 + mt + phase + inter, random =~ 1 + mt + inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.level.slope$model <- lme(values ~ 1 + mt + phase + inter, random =~ 1 + phase +inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend.level.slope$model <- lme(values ~ 1 + mt + phase + inter, random =~ 1 + mt + phase +inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.nointercept.trend.level.slope$model <- lme(values ~ 1 + mt + phase + inter, random =~ -1 + mt + phase +inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend$LR.test <- anova(out$random.level.slope$model, out$random.trend.level.slope$model)
    out$random.level$LR.test <- anova(out$random.trend.slope$model, out$random.trend.level.slope$model)
    out$random.slope$LR.test <- anova(out$random.trend.level$model, out$random.trend.level.slope$model)
    out$random.nointercept.trend.level.slope$LR.test <- anova(out$random.nointercept.trend.level.slope$model, out$random.trend.level.slope$model)
  }

  if(ICC) {
    out$model.0 <- lme(values ~ 1, random =~1|case, data = dat, method = method, na.action=na.omit, control = control)
    VC <- as.numeric(VarCorr(out$model.0))
    out$ICC <- VC[1]/(VC[1]+VC[2])	
    out$model.without <- gls(values ~ 1, data = dat, method = method, na.action=na.omit, control = control)
    dif <- anova(out$model.0, out$model.without)
    out$L.ICC <- dif$L.Ratio[2]
    out$p.ICC <- dif$"p-value"[2]
  } 
  
  class(out) <- c("sc","hplm")
  
  out
}
