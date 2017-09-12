

smoothSC <- function(data, FUN = "movingMedian", intensity = NULL){
  data <- .SCprepareData(data)
  ATTRIBUTES <- attributes(data)
  NAMES <- names(data)
  if (FUN == "movingMean") {
    if(is.null(intensity)) 
      intensity <- 1
    out <-lapply(data, function(x) {
      x$values <- .SCmovingAverage(x$values, intensity, mean)
      x})
  }
  if (FUN == "movingMedian") {
    if(is.null(intensity)) 
      intensity <- 1
    out <- lapply(data, function(x) {
      x$values <- .SCmovingAverage(x$values, intensity, median)
      x})
  }
  if (FUN == "localRegression") {
    if(is.null(intensity)) 
      intensity <- 0.2
    out <- lapply(data, function(x) {
      xval <- x$mt[!is.na(x$values)]
      yval <- x$values[!is.na(x$values)]
      x$values <- lowess(yval~xval, f = intensity)$y
      x})
  }
  attributes(out) <- ATTRIBUTES
  names(out) <- NAMES
  out
}	
