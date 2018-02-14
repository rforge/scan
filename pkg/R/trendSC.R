#' Trend analysis for single-cases data
#' 
#' The \code{trendSC} function provides an overview of linear trends in
#' single-case data.  By default, it gives you the intercept and slope of a
#' linear and a squared regression of measurement-time on scores.  Models are
#' computed separately for the A phase, the B-phase, and the whole data.  For a
#' more advanced application, you can add regression models using the R
#' specific formula class.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{makeSCDF}} to learn
#' about this format.
#' @param B.offset An offset for the first phase B measurement-time (MT). If
#' set \code{B.offset = 0}, the first phase B measurement is handled as MT 1.
#' Default is \code{B.offset = -1}, making the first value of phase B MT = 0.
#' @param model A string or a list of (named) strings each depicting one
#' regression model. This is a formula expression of the standard R class. The
#' parameters of the model are \code{values}, \code{mt} and \code{phase}.
#' @return \item{trend}{A matrix containing the results (Intercept, B and beta)
#' of separate regression models for phase A, phase B, and the whole data.}
#' \item{B.offset}{Numeric argument from function call (see \code{Arguments}
#' section).}
#' @author Juergen Wilbert
#' @seealso \code{\link{describeSC}}, \code{\link{overlapSC}},
#' \code{\link{plm}}, \code{\link{hplm}}
#' @examples
#' 
#' ## Compute the linear and squared regression for a random single-case
#' matthea <- rSC(slope = 0.5)
#' trendSC(matthea)
#' 
#' ## Besides the linear and squared regression models compute two custom models:
#' ## a) a cubic model, and b) the values predicted by the natural logarithm of the
#' ## measurement time.
#' ben <- rSC(slope = 0.3)
#' trendSC(ben, B.offset = 0, model = c("Cubic" = "values ~ I(mt^3)", "Log Time" = "values ~ log(mt)"))
#' 
trendSC <- function(data, B.offset = -1,model = NA) {
  phase <- NULL
  data <- .SCprepareData(data)
  
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculations can only be applied to one single-case data set.\n")
  
  data <- data[[1]]
  
  data.A <- subset(data, phase == "A")
  data.B <- subset(data, phase == "B")
  data.B$mt <- data.B$mt - min(data.B$mt) + 1 + B.offset
  
  row.names <- c("Linear.AB","Linear.A","Linear.B","Squared.AB","Squared.A","Squared.B")
  rows <- length(row.names)
  
  out <- c(
    .SCbeta(lm(values~mt, data = data)), 
    .SCbeta(lm(values~mt, data = data.A)),
    .SCbeta(lm(values~mt, data = data.B)),
    .SCbeta(lm(values~I(mt^2), data = data)),
    .SCbeta(lm(values~I(mt^2), data = data.A)),
    .SCbeta(lm(values~I(mt^2), data = data.B))
  )
  
  if(!is.na(model[1])) {
    for(i in 1:length(model)) {
      out <- c(out,
               .SCbeta(lm(as.formula(model[i]), data = data)),
               .SCbeta(lm(as.formula(model[i]), data = data.A)),
               .SCbeta(lm(as.formula(model[i]), data = data.B))
      )
    }
    rows <- rows + length(model) * 3
    if(is.null(names(model)))
      names(model) <- model
    row.names <- c(row.names,paste(rep(names(model), each = 3), rep(c("AB","A","B")), sep = "."))
  }
  
  out <- matrix(out,rows,3, byrow = TRUE, dimnames = list(row.names, c("Intercept", "B","Beta")))
  out <- list(trend = out, B.offset = B.offset)
  class(out) <- c("sc","trend")
  out
}

