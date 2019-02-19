#' Percent exceeding the median
#' 
#' The \code{pem} function returns the percentage of phase B data exceeding the
#' phase A median.  Additionally, a chi square test against a 50/50
#' distribution is computed.  Different measures of central tendency can be
#' addressed for alternative analyses.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param dvar Character string with the name of the independend variable.
#' @param pvar Character string with the name of the phase variable.
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @param binom.test Computes a binomial test for a 50/50 distribution. Default
#' is \code{binom.test = TRUE}.
#' @param chi.test Computes a Chi-square test. The default setting
#' \code{chi.test = FALSE} skips the Chi-square test.
#' @param FUN Data points are compared with the phase A median. Use this
#' argument to implement alternative measures of central tendency. Default is
#' \code{FUN = median}
#' @param \dots Additional arguments for the \code{FUN} parameter (e.g.
#' \code{FUN = mean, trim = 0.1} will use the 10 percent trimmed arithmetic
#' mean instead of the median for comparisons).
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second to the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @return \item{PEM}{Percent exceeding the median.} \item{test}{A list of
#' results from the binomial- and chi-square test.} \item{decreasing}{Logical
#' argument from function call (see \code{Arguments} above).}
#' @author Juergen Wilbert
#' @seealso \code{\link{overlapSC}}, \code{\link{describeSC}},
#' \code{\link{nap}}, \code{\link{pand}}, \code{\link{pet}}, \code{\link{pnd}}
#' @examples
#' 
#' ## Calculate the PEM including the Binomial and Chi-square tests for a single-case
#' dat <- rSC(5, level = 0.5)
#' pem(dat, chi.test = TRUE)
#' 
pem <- function(data, dvar = NULL, pvar = NULL, decreasing = FALSE, binom.test = TRUE, chi.test = FALSE, FUN = median, phases = c("A","B"), ...) {

  if(!is.null(dvar)) 
    attr(data, .opt$dv) <- dvar
  else
    dvar <- attr(data, .opt$dv)
  
  if(!is.null(pvar))
    attr(data, .opt$phase) <- pvar
  else
    pvar <- attr(data, .opt$phase)
  
  data <- .SCprepareData(data, na.rm = TRUE, change.var.values = FALSE, change.var.phase = FALSE)
  data <- .keepphasesSC(data, phases = phases,pvar = pvar)$data
  
  N <- length(data)
  
  PEM       <- rep(NA,N)
  chi       <- rep(NA,N)
  chi.df    <- rep(NA,N)
  chi.p     <- rep(NA,N)
  binom.p   <- rep(NA,N)
  positives <- rep(NA,N)
  total     <- rep(NA,N)
  
  
  for(i in 1:N) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    if (!decreasing)
      PEM[i] <- mean(B > FUN(A, na.rm = TRUE,...), na.rm = TRUE) * 100
    if (decreasing)
      PEM[i] <- mean(B < FUN(A, na.rm = TRUE,...), na.rm = TRUE) * 100
    if(binom.test) {
      nB <- length(B)
      bi <- binom.test(round(PEM[i] / 100  * nB), nB, alternative = "greater")
      positives[i] <- bi$statistic
      total[i]     <- bi$parameter
      binom.p[i]   <- bi$p.value
    }
    if(chi.test) {
      nB <- length(B)
      exceeding <- PEM[i] / 100  * nB
      res <- chisq.test(c(exceeding, nB - exceeding), p = c(0.5,0.5))
      chi[i]    <- res$statistic
      chi.df[i] <- res$parameter
      chi.p[i]  <- res$p.value
    }
  }
  stats.ma <- cbind(positives, total,binom.p)
  colnames(stats.ma) <- c("positives","total","binom.p")
  rownames(stats.ma) <- names(data)
  if(chi.test) {
    cn <- c(colnames(stats.ma),"Chi", "DF", "p")
    stats.ma <- cbind(stats.ma, chi, chi.df, chi.p)
    colnames(stats.ma) <- cn
  }
  
  out <- list(PEM = PEM, test = stats.ma, decreasing = decreasing)
  class(out) <- c("sc","PEM")
  out
}
