#' Percentage of non-overlapping data
#' 
#' This function returns the percentage of non-overlapping data.  Due to its
#' error-proneness the PND should not be used, but \code{\link{nap}} or
#' \code{\link{pand}} instead (see Parker & Vannest, 2009).
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
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
pnd <- function(data, decreasing = FALSE, phases = c("A","B")) {
  
  data <- .SCprepareData(data)
  data <- .keepphasesSC(data, phases = phases)$data

  PND <- c()
  for(i in 1:length(data)) {
    A <- data[[i]][,2][data[[i]][,1] == "A"]
    B <- data[[i]][,2][data[[i]][,1] == "B"]
    if (!decreasing)
      PND[i] <- sum(B > max(A, na.rm = TRUE), na.rm = TRUE) / length(B) * 100
    if (decreasing)
      PND[i] <- sum(B < min(A, na.rm = TRUE), na.rm = TRUE) / length(B) * 100
  }
  out <- list(PND = PND)
  class(out) <- c("sc","PND")
  out
}
