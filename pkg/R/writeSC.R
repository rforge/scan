#' Export data into a .csv-file
#' 
#' This function restructures and exports single-case data into a .csv-file.
#' 
#' 
#' @param dat A single-case data frame or a list of single-case data frames.
#' See \code{\link{makeSCDF}} to learn about this format.
#' @param filename A character string defining the output file name (e.g.
#' \code{"SC_data.csv"}.
#' @param sep The field separator string. Values within rows will be separated
#' by this string. Default is \code{sep = ","}.
#' @param dec The string used for decimal points. Must be a single character.
#' Default is \code{dec = "."}
#' @param \dots Further arguments passed to write.table.
#' @author Juergen Wilbert
#' @seealso \code{\link{write.table}}, \code{\link{readSC}},
#' \code{\link{makeSCDF}}
#' @keywords manip
#' @examples
#' 
#' ## Write single-case data to a .csv-file
#' jessica <- rSC(level = .5)
#' writeSC(jessica, "SCdata_Jessica.csv")
#' 
#' ## Write multiple cases to a .csv-file with semicolon as field and comma as decimal separator
#' writeSC(Grosche2011, "MBDdata_Grosche.csv", sep = ";", dec = ",")
#' 
writeSC <- function(dat, filename = NULL, sep = ",", dec = ".", ...) {
  if(is.null(filename)) {
    filename <- file.choose()
    cat("Write to file", filename,"\n\n")
  }
  
  write.table(longSCDF(dat), file = filename, sep = sep, row.names = FALSE, dec = dec, ...)
  
}
