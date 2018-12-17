#' Nonoverlap of all Pairs
#' 
#' The \code{nap} function calculates the nonoverlap of all pairs (NAP; Parker
#' & Vannest, 2009).  NAP summarizes the overlap between all pairs of phase A
#' and phase B data points.  If an increase of phase B scores is expected, a
#' non-overlapping pair has a higher phase B data point.  The NAP equals
#' \eqn{number of pairs showing no overlap / number of pairs}.  Because NAP can
#' only take values between 50 and 100 percent, a rescaled and therefore more
#' intuitive NAP (0-100\%) is also displayed.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second to the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @return \item{NAP}{Nonoverlap of all pairs.} \item{Rescaled NAP}{NAP
#' rescaled to 0-100\%.}
#' @author Juergen Wilbert
#' @seealso \code{\link{overlapSC}}, \code{\link{describeSC}},
#' \code{\link{pand}}, \code{\link{pem}}, \code{\link{pet}}, \code{\link{pnd}}
#' @references Parker, R. I., & Vannest, K. (2009). An improved effect size for
#' single-case research: Nonoverlap of all pairs. \emph{Behavior Therapy, 40},
#' 357-367.
#' @examples
#' 
#' ## Calculate NAP for a study with  lower expected phase B scores (e.g. aggressive behavior)
#' gretchen <- scdf(c(12,14,9,10,10,6,4,5,3,4), B.start = 5)
#' nap(gretchen, decreasing = TRUE)
#' 
#' ## Request NAP for all cases fom the Grosche2011 data
#' lapply(Grosche2011, nap)
#' 

nap <- function(data, decreasing = FALSE, phases = c("A","B")) {
  data <- .SCprepareData(data)
  data <- .keepphasesSC(data, phases = phases)$data
  
  N <- length(data)
  
  NAP   <- c()
  PAIRS <- c()
  POS   <- c()
  TIES  <- c()
  
  for(case in 1:N) {
    df <- data[[case]]

    A     <- df[df[,"phase"] == "A", "values"]
    B     <- df[df[,"phase"] == "B", "values"]
    pairs <- length(A) * length(B)
    
    if (!decreasing)
      pos <- pairs - sum(sapply(A,function(x)x>=B), na.rm = TRUE)
    if (decreasing)
      pos <- pairs - sum(sapply(A,function(x)x<=B), na.rm = TRUE)
    
    ties <- sum(sapply(A,function(x)x==B), na.rm = TRUE)
    
    NAP   <- c(NAP, (pos + (0.5 * ties)) / pairs)
    TIES  <- c(TIES, ties)
    PAIRS <- c(PAIRS, pairs)
    POS   <- c(POS, pos)
    
  }  
  nap <- data.frame(Case = names(data), NAP = NAP*100, Rescaled = 2 * (NAP*100) - 100, Pairs = PAIRS, Positives = POS, Ties = TIES)  

  out <- list(nap = nap, N = N)
  class(out) <- c("sc","NAP")
  out
}
