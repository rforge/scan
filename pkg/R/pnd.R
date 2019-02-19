#' Percentage of non-overlapping data
#' 
#' This function returns the percentage of non-overlapping data.  Due to its
#' error-proneness the PND should not be used, but \code{\link{nap}} or
#' \code{\link{pand}} instead (see Parker & Vannest, 2009).
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param dvar Character string with the name of the independend variable.
#' @param pvar Character string with the name of the phase variable.
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second and the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @return \item{PND}{Percentage of non-overlapping data.}
#' @author Juergen Wilbert
#' @seealso \code{\link{overlapSC}}, \code{\link{describeSC}},
#' \code{\link{nap}}, \code{\link{pand}}, \code{\link{pem}}, \code{\link{pet}}
#' @examples
#' 
#' ## Calculate the PND for multiple single-case data
#' pnd(GruenkeWilbert2014)
#' 
pnd <- function(data, dvar = NULL, pvar = NULL, decreasing = FALSE, phases = c("A","B")) {

  if(!is.null(dvar)) 
    attr(data, .opt$dv) <- dvar
  else
    dvar <- attr(data, .opt$dv)
  
  if(!is.null(pvar))
    attr(data, .opt$phase) <- pvar
  else
    pvar <- attr(data, .opt$phase)
  
  data <- .SCprepareData(data, na.rm = TRUE, change.var.values = FALSE, change.var.phase = FALSE)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  PND <- c()
  n.B <- c()
  
  for(i in 1:length(data)) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    n.B[i] <- length(B)
    if (!decreasing)
      PND[i] <- sum(B > max(A, na.rm = TRUE), na.rm = TRUE) /  n.B[i] * 100
    if (decreasing)
      PND[i] <- sum(B < min(A, na.rm = TRUE), na.rm = TRUE) /  n.B[i] * 100
  }
  
  out <- list(PND = PND, case.names = names(data), n.B = n.B)
  class(out) <- c("sc","PND")
  out
}
