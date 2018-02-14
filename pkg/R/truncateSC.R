#' Truncate single-case data
#' 
#' This function truncates data points at the beginning and / or end each phase.
#' 
#' 
#' @param data A single-case data frame or a list of single-case data frames.
#' See \code{\link{scdf}} to learn about this format.
#' @param truncate A list with a vector of two (beginning and end) values for each phase defining the number of data points to be deleted.
#' For lists of single-case data frames, the truncation is adapted to the length 
#' of each phase for each single case.
#' @return A truncated data frame (for each single-case).
#' @author Juergen Wilbert
#' @seealso \code{\link{outlierSC}}, \code{\link{fillmissingSC}},
#' \code{\link{scdf}}
#' @keywords manip
#' @examples
#' 
#' # Truncate the first two data points of both phases and compare the two data sets
#' truHeart <- truncateSC(byHeart2011[1], list(A = c(2,0), B = c(2,0)))
#' plotSC(c(original = byHeart2011[1], selected = truHeart))
#' 
truncateSC <- function (data, truncate = list(A = c(0,0), B = c(0,0))){
  data <- .SCprepareData(data)
  N = length(data)
  
  cat("Deletet measurements per case:\n\n")
  for(i in 1:N){
    phases <- rle(as.character(data[[i]]$phase))
    phases$start <- c(1,cumsum(phases$lengths)+1)[1:length(phases$lengths)]
    phases$stop <- cumsum(phases$lengths)
    class(phases) <- "list"
    deselect <- c()
    for(ph in 1:length(phases$values)) {
      if(truncate[[ph]][1] > 0)
        deselect <- c(deselect, phases$start[ph] : (phases$start[ph] + truncate[[ph]][1] - 1))
      if(truncate[[ph]][2] > 0)
        deselect <- c(deselect, (phases$stop[ph]  - truncate[[ph]][2] + 1) : phases$stop[ph])
    }
    
    cat(paste0(names(data)[i], ": "))
    cat(deselect)
    cat("\n")
    data[[i]] <- data[[i]][-deselect,]
  }
  return(data)
}
