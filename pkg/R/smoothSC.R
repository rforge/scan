#' Smoothing single-case data
#' 
#' The \code{smoothSC} function provides procedures to smooth single-case data
#' (i.e., to eliminate noise). A moving average function (mean- or
#' median-based) replaces each data point by the average of the surrounding
#' data points step-by-step. With a local regression function, each data point
#' is regressed by its surrounding data points.
#' 
#' @inheritParams .inheritParams
#' @param FUN Function determining the smoothed scores. Default \code{FUN =
#' "movingMedian"} is a moving Median function. Further possible values are:
#' \code{"movingMean"} and a non-parametric \code{"localRegression"}.
#' @param intensity For \code{FUN = "movingMedian"} and \code{"movingMean"} it
#' is the lag used for computing the average. Default is \code{intensity = 1}.
#' In case of \code{FUN = "localRegression"} it is the proportion of
#' surrounding data influencing each data point, which is \code{intensity =
#' 0.2} by default.
#' @return Returns a data frame (for each single-case) with smoothed data
#' points. See \code{\link{scdf}} to learn about the format of these data
#' frames.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#' 
#' ## Use the three different smoothing functions and compare the results
#' study <- c(Huber2014$Berta,
#'            smoothSC(Huber2014$Berta, FUN = "movingMedian"),
#'            smoothSC(Huber2014$Berta, FUN = "movingMean"),
#'            smoothSC(Huber2014$Berta, FUN = "localRegression"))
#' names(study) <- c("Original","Moving Median","Moving Mean", "Local Regression")
#' plot(study)
#' 
#' @export
smoothSC <- function(data, dvar, mvar, FUN = "movingMedian", intensity = NULL){
  if (missing(dvar)) dvar <- attr(data, .opt$dv) else attr(data, .opt$dv) <- dvar
  if (missing(mvar)) mvar <- attr(data, .opt$mt) else attr(data, .opt$mt) <- mvar
  
  data <- .SCprepareData(data, change.var.values = FALSE, change.var.mt = FALSE)
  ATTRIBUTES <- attributes(data)
  NAMES <- names(data)
  if (FUN == "movingMean") {
    if(is.null(intensity)) 
      intensity <- 1
    out <-lapply(data, function(x) {
      x[, dvar] <- .SCmovingAverage(x[, dvar], intensity, mean)
      x})
  }
  if (FUN == "movingMedian") {
    if(is.null(intensity)) 
      intensity <- 1
    out <- lapply(data, function(x) {
      x[, dvar] <- .SCmovingAverage(x[, dvar], intensity, median)
      x})
  }
  if (FUN == "localRegression") {
    if(is.null(intensity)) 
      intensity <- 0.2
    out <- lapply(data, function(x) {
      xval <- x[!is.na(x[, dvar]), mvar]
      yval <- x[!is.na(x[, dvar]), dvar]
      x[, dvar] <- lowess(yval~xval, f = intensity)$y
      x})
  }
  attributes(out) <- ATTRIBUTES
  names(out) <- NAMES
  out
}	
