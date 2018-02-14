#' Smoothing single-case data
#' 
#' The \code{smoothSC} function provides procedures to smooth single-case data
#' (i.e., to eliminate noise). A moving average function (mean- or
#' median-based) replaces each data point by the average of the surrounding
#' data points step-by-step. With a local regression function, each data point
#' is regressed by its surrounding data points.
#' 
#' 
#' @param data A single-case data frame or a list of single-case data frames.
#' See \code{\link{scdf}} to learn about this format.
#' @param FUN Function determining the smoothed scores. Default \code{FUN =
#' "movingMedian"} is a moving Median function. Further possible values are:
#' \code{"movingMean"} and a non-parametric \code{"localRegression"}.
#' @param intensity For \code{FUN = "movingMedian"} and \code{"movingMean"} it
#' is the lag used for computing the average. Default is \code{intensity = 1}.
#' In case of \code{FUN = "localRegression"} it is the proportion of
#' surrounding data influencing each data point, which is \code{intensity =
#' 0.2} by default.
#' @return Returns a data frame (for each single-case) with smoothed data
#' points. See \code{\link{makeSCDF}} to learn about the format of these data
#' frames.
#' @author Juergen Wilbert
#' @keywords manip
#' @examples
#' 
#' ## Use the three different smoothing functions and compare the results
#' berta_mmd <- smoothSC(Huber2014$Berta)
#' berta_mmn <- smoothSC(Huber2014$Berta, FUN = "movingMean")
#' berta_lre <- smoothSC(Huber2014$Berta, FUN = "localRegression")
#' plotSC(c("Original" = Huber2014$Berta,"Moving Median" = berta_mmd,
#'     "Moving Mean" = berta_mmn,"Local Regression" = berta_lre))
#' 
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
