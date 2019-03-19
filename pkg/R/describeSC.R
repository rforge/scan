#' Descriptive statistics for single-case data
#' 
#' The \code{describeSC} function provides common descriptive statistics for
#' single-case data.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about this format.
#' @param dvar Character string with the name of the dependent variable. Defaults to the attributes in the scdf file.
#' @param pvar Character string with the name of the phase variable. Defaults to the attributes in the scdf file.
#' @param mvar Character string with the name of the measurement time variable. Defaults to the attributes in the scdf file.
#' @return A data frame of descriptive statistics (for each single-case), i.e.:
#' number of observations, number of missing values, measures of central
#' tendency, variation, and trend.
#' @author Juergen Wilbert
#' @seealso \code{\link{overlapSC}}, \code{\link{plotSC}}
#' @examples
#' 
#' 
#' ## Descriptive statistics for a study of three single-cases
#' describeSC(Grosche2011)
#' 
#' ## Descriptives of a three phase design
#' describeSC(exampleABC)
#' 
#' ## Write descriptive statistics to .csv-file
#' study <- describeSC(Waddell2011)
#' write.csv(study$descriptives, file = "descriptives.csv")
#' 
#' @export
describeSC <- function(data, dvar, pvar, mvar) {
 
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- attr(data, .opt$dv) else attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- attr(data, .opt$phase) else attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- attr(data, .opt$mt) else attr(data, .opt$mt) <- mvar
  
  data.list <- .SCprepareData(data, change.var.values = FALSE, change.var.mt = FALSE, change.var.phase = FALSE)
  
  N <- length(data.list)
  case.names <- names(data.list)

  design <- rle(as.character(data.list[[1]][,pvar]))$values

  while (any(duplicated(design))) {
    design[anyDuplicated(design)] <-
      paste0(design[anyDuplicated(design)], ".phase", anyDuplicated(design))
  }
  
  VAR <- c("n","mis","m","md","sd","mad","min","max","trend")
  VAR2 <- paste0(rep(VAR, each = length(design)),".",design)
  
  d.f <- as.data.frame(matrix(nrow = N, ncol = length(VAR2)))
  colnames(d.f) <- VAR2
  rownames(d.f) <- case.names
  
  for(case in 1:N) {
    data <- data.list[[case]]
    for(i in 1:length(design)) {
      phases <- .phasestructure(data, pvar = pvar)
 
      x <- data[phases$start[i]:phases$stop[i], mvar]
      y <- data[phases$start[i]:phases$stop[i], dvar]
      phase <- design[i]

      d.f[case, paste0("n.",phase)]     <- length(y)
      d.f[case, paste0("mis.",phase)]   <- sum(is.na(y),na.rm = TRUE) 
      d.f[case, paste0("m.",phase)]     <- mean(y,na.rm = TRUE) 
      d.f[case, paste0("md.",phase)]    <- median(y,na.rm = TRUE) 
      d.f[case, paste0("sd.",phase)]    <- sd(y,na.rm = TRUE)
      d.f[case, paste0("mad.",phase)]   <- mad(y,na.rm = TRUE) 
      d.f[case, paste0("min.",phase)]   <- min(y,na.rm = TRUE) 
      d.f[case, paste0("max.",phase)]   <- max(y,na.rm = TRUE) 
      d.f[case, paste0("trend.",phase)] <- coef(lm(y~I(x-x[1]+1)))[2]
    }
  }
  
  out <- list(descriptives = d.f,
              design = design,
              N = N)
  class(out) <- c("sc", "describe")
  attr(out, "var.phase")  <- pvar
  attr(out, "var.mt")     <- mvar
  attr(out, "var.values") <- dvar
  
  out
}
