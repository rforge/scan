#' Creating a long format data frame from several single-case data frames
#' (scdf).
#' 
#' The longSCDF function transposes a scdf into one long data frame.
#' Additionally, a data frame can be merged that includes data of the subjects.
#' This might be helpful to prepare data to be used with other packages than scan.
#' 
#' 
#' @param data A single-case data frame (scdf).
#' @param l2 A dataframe providing additional variables at Level 2. The scdf
#' has to have names for all cases and the Level 2 dataframe has to have a
#' column named 'cases' corresponding case names.
#' @param id Variable name of the Level 2 data frame that contains the case
#' names.
#' @param model If set, calculates interaction terms (see 'method' argument in
#' plm function).
#' @param ... Additional arguments provided to the internally used merge function.
#' @param check Indicating whether the scd is checked.
#' @return Returns one data frame with data of all single-cases structured by
#' the variable 'case'.
#' @author Juergen Wilbert
#' @seealso \code{\link{scdf}}
#' \code{\link{writeSC}}
#' @keywords manip
#' @examples
#' 
#' ## Convert the list of three single-case data frames from Grosche (2011) into one long data frame
#' Grosche2011
#' Grosche2011_long <- longSCDF(Grosche2011)
#' Grosche2011_long
#' 
longSCDF <- function(data, l2 = NULL, id = "case", model = NULL, check = TRUE, ...) {
  if(check)
    dat <- .SCprepareData(data) 
  else 
    dat <- data
  
  label <- names(dat)
  if (is.null(label))
    label <- as.character(1:length(dat))
  outdat <- vector()
  
  
  if(!is.null(model)) {
    for(case in 1:length(dat)) {
      data.inter <- .plm.dummy(dat[[case]], model = model)
      dat[[case]]$mt <- data.inter$mt
      dat[[case]] <- cbind(dat[[case]],data.inter[,-1])
    }
  }
  
  
  for (case in 1:length(dat)) {
    dat[[case]]$case <- label[case]
    outdat <- rbind(outdat, dat[[case]])
  }
  
  outdat <- cbind(outdat[,ncol(outdat)],outdat[,-ncol(outdat)])
  colnames(outdat)[1] <- "case"
  
  if(!is.null(l2)) {
    outdat <- merge(outdat, l2, by = id, ...)
  }
  return(outdat)
  
}
