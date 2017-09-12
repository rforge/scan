writeSC <- function(dat, filename = NULL, sep = ",", dec = ".", ...) {
  if(is.null(filename)) {
    filename <- file.choose()
    cat("Write to file", filename,"\n\n")
  }
  
  write.table(longSCDF(dat), file = filename, sep = sep, row.names = FALSE, dec = dec, ...)
  
}