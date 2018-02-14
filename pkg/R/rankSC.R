#' Rank-transformation of single-case data files
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' 
#' @return An \code{scdf} object where the values of the \code{values} variable(s) are replaced with ranks.
#' 
#' @author Juergen Wilbert
#' @examples
#' Huber2014_rank <- rankSC(Huber2014)
#' plot(Huber2014_rank, style = "grid2")

rankSC <- function(data) {
  data.list <- .SCprepareData(data)
  N <- length(data.list)

  for(i in 1:N) {
    data.list[[i]]$values <- rank(data.list[[i]]$values)
  }
  class(data.list) <- c("scdf","list")
  
  data.list
}

