#' Rank-transformation of single-case data files
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param var A charcter or character with the names of the variables to be ranked.
#' @param grand If TRUE, ranks will be calculated across all cases. If FALSE ranks are calculated within each case.
#' @param ... Additional paramters passed to the \code{\link{rank}} function.
#' 
#' @return An \code{scdf} object where the values of the variable(s) are replaced with ranks.
#' 
#' @author Juergen Wilbert
#' @examples
#' Huber2014_rank <- rankSC(Huber2014, var = "values")
#' plot(Huber2014_rank, style = "grid2")
#' 
#' @export

rankSC <- function(data, var = NULL, grand = TRUE, ...) {
  
  if(isFALSE(grand)) {
    for(i in 1:length(data)) {
      for(v in var)
        data[[i]][, v] <- rank(data[[i]][,var], ...)
    }
  }

  if(isTRUE(grand)) {
    ATTRIBUTES <- attributes(data)
    data <- longSCDF(data)
    data$case <- factor(data$case, levels = unique(data$case))
    for(v in var)
      data[, v] <- rank(data[,var],...)
    data <- split(data, data$case)
    data <- lapply(data, function(x) x[,-1])
    attributes(data) <- ATTRIBUTES
  }
    
  data
}

