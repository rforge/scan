#' Descriptive statistics for single-case data
#' 
#' The \code{describeSC} function provides common descriptive statistics for
#' single-case data.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about this format.
#' @return A data frame of descriptive statistics (for each single-case), i.e.:
#' number of observations, number of missing values, measures of central
#' tendency, variation, and trend.
#' @author Juergen Wilbert
#' @seealso \code{\link{overlapSC}}, \code{\link{plotSC}},
#' \code{\link{writeSC}}
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
describeSC <- function(data) {
  data.list <- .SCprepareData(data)
  N <- length(data.list)
  case.names <- names(data.list)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  
  design <- rle(as.character(data.list[[1]]$phase))$values
  
  while(any(duplicated(design))) {
    design[anyDuplicated(design)] <- paste0(design[anyDuplicated(design)],".phase",anyDuplicated(design))
  }
  
  VAR <- c("n","mis","m","md","sd","mad","min","max","trend")
  
  VAR2 <- paste0(rep(VAR, each = length(design)),".",design)
  
  d.f <- as.data.frame(matrix(nrow = N, ncol = length(VAR2)))
  colnames(d.f) <- VAR2
  rownames(d.f) <- case.names
  
  for(case in 1:N) {
    data <- data.list[[case]]
    for(i in 1:length(design)) {
      phases <- rle(as.character(data$phase))
      phases$start <- c(1,cumsum(phases$lengths)+1)[1:length(phases$lengths)]
      phases$stop <- cumsum(phases$lengths)
      class(phases) <- "list"
      x <- data$mt[phases$start[i]:phases$stop[i]]
      y <- data$values[phases$start[i]:phases$stop[i]]
      phase <- design[i]

      d.f[case, paste0("n.",phase)] <- length(y)
      d.f[case, paste0("mis.",phase)] <- sum(is.na(y),na.rm = TRUE) 
      d.f[case, paste0("m.",phase)] <- mean(y,na.rm = TRUE) 
      d.f[case, paste0("md.",phase)] <- median(y,na.rm = TRUE) 
      d.f[case, paste0("sd.",phase)] <- sd(y,na.rm = TRUE)
      d.f[case, paste0("mad.",phase)] <- mad(y,na.rm = TRUE) 
      d.f[case, paste0("min.",phase)] <- min(y,na.rm = TRUE) 
      d.f[case, paste0("max.",phase)] <- max(y,na.rm = TRUE) 
      d.f[case, paste0("trend.",phase)] <- coef(lm(y~I(x-x[1]+1)))[2]
    }
  }
  
  out <- list(descriptives = d.f, design = design, N = N)
  class(out) <- c("sc","describe")
  out
}
