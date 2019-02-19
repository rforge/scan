#' Autocorrelation for single-case data
#' 
#' The autocorrSC function calculates autocorrelations within each phase and
#' across all phases.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about this format.
#' @param dvar Character string with the name of the independend variable.
#' @param pvar Character string with the name of the phase variable.
#' @param mvar Character string with the name of the measurement time variable.
#' @param lag.max The lag up to which autocorrelations will be computed.
#' Default is \code{lag.max = 3}.
#' @param ... Further arguments passed to the \code{\link{acf}} function
#' @return autocorr A data frame containing separate autocorrelations for each
#' phase and for all phases (for each single-case). If \code{lag.max} exceeds
#' the length of a phase minus one, NA is returned for this cell.
#' @author Juergen Wilbert
#' @seealso \code{\link{trendSC}}, \code{\link{plm}}, \code{\link{acf}}
#' @examples
#' 
#'  
#' ## Compute autocorrelations for a list of four single-cases with max.lag = 2
#' autocorrSC(Huber2014, lag.max = 2)
#' 
#' @concept Autocorrelation
#' @concept Seiral correlation

autocorrSC <- function(data, dvar = NULL, pvar = NULL, mvar = NULL, lag.max = 3, ...) {
  
  if(!is.null(dvar)) 
    attr(data, .opt$dv) <- dvar
  else
    dvar <- attr(data, .opt$dv)
  
  if(!is.null(pvar))
    attr(data, .opt$phase) <- pvar
  else
    pvar <- attr(data, .opt$phase)
  
  if(!is.null(mvar))
    attr(data, .opt$mt) <- mvar
  else
    mvar <- attr(data, .opt$mt)
  
  data <- .SCprepareData(data, change.var.values = FALSE, change.var.mt = FALSE, change.var.phase = FALSE)
  
  N <- length(data)
  case.names <- names(data)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  VAR <- paste0("lag_",1:lag.max)
  
  design <- rle(as.character(data[[1]][,pvar]))$values
  
  while(any(duplicated(design))) {
    design[anyDuplicated(design)] <- paste0(design[anyDuplicated(design)],".phase",anyDuplicated(design))
  }
  
  
  ac <- data.frame(case = rep(case.names, each = length(design) + 1), phase = rep(c(design, "all"), N))
  ac[, VAR] <- NA
  
  
  for(case in 1:N) {
    phases <- .phasestructure(data[[case]], pvar = pvar)
    
    for(phase in 1:length(design)) {
      y <- data[[case]][phases$start[phase]:phases$stop[phase],dvar]
      if(length(y) - 1 < lag.max) lag <- length(y) - 1 else lag <- lag.max
      
      ac[(case - 1) * (length(design) + 1) + phase, VAR[1:lag]] <- acf(y, lag.max = lag, plot = FALSE, ...)$acf[-1]
    }
    y <- data[[case]][,dvar]
    if(length(y) - 1 < lag.max) lag <- length(y) - 1 else lag <- lag.max
    
    ac[(case - 1) * (length(design) + 1) + (length(design) + 1), VAR[1:lag]] <- acf(y, lag.max = lag, plot = FALSE, ...)$acf[-1]
    
  }
 
  out <- list(autocorr = ac, dvar = dvar)
  class(out) <- c("sc","autocorr")
  out
}
