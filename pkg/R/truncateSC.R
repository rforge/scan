#' Truncate single-case data
#' 
#' This function truncates data points at the beginning and / or end each phase.
#' 
#' @inheritParams .inheritParams
#' @param truncate A list with a vector of two (beginning and end) values for each phase defining the number of data points to be deleted.
#' For lists of single-case data frames, the truncation is adapted to the length 
#' of each phase for each single case.
#' @return A truncated data frame (for each single-case).
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#' 
#' # Truncate the first two data points of both phases and compare the two data sets
#' study <- c(byHeart2011[1], truncateSC(byHeart2011[1], truncate = list(A = c(2,0), B = c(2,0))))
#' names(study) <- c("Original","Selected")
#' plot(study)
#' 
#' @export
truncateSC <- function (data, pvar, truncate = list(A = c(0,0), B = c(0,0))){
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar

  data <- .SCprepareData(data)
  N = length(data)
  
  cat("Deletet measurements per case:\n\n")
  for(i in 1:N) {
    phases        <- rle(as.character(data[[i]][ ,pvar]))
    phases$start  <- c(1, cumsum(phases$lengths) + 1)[1 : length(phases$lengths)]
    phases$stop   <- cumsum(phases$lengths)
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
    data[[i]] <- data[[i]][-deselect, ]
  }
  return(data)
}
