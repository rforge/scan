#' Single case data frame
#' 
#' The class \code{scdf} stores single-case study data with one or more
#' single-cases.
#' 
#' The \code{scdf} class is a wrapper for a list containing a dataframe for
#' each case. Each of these dataframes has to contain columns with the names
#' 'phase' and 'values'. A third column 'mt' contains the mesaurement times and
#' if not provided is set to \code{1:nrow} by all \code{scan} functions.
#' Methods for the \code{sdf} class are \code{print}, \code{summary}, and
#' \code{plot}.
#' 
#' @aliases scdf scdf-class summary.scdf as.scdf c.scdf checkSCDF
#' makeSCDF
#' @param values A vector containing measurement values of the target variable.
#' @param B.start The first measurement of phase B (simple coding if design is
#' strictly AB).
#' @param mt A vector defining measurement times. Default is \code{MT =
#' (1,2,3,...,n)}.
#' @param phase.design A vector defining the length and label of each phase.
#' E.g., \code{phase.length = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)}.
#' @param name A name for the case.
#' @param phase A vector defining phase assignment.
#' @param dvar A character string with the name of the independent variable.
#' @param var.values Deprecated. Please use dvar paramter instead
#' @param pvar A character string with the name of the phase variable.
#' @param mvar A character string with the name of the measurement-time variable.
#' @param ...  Additional variables. E.g., \code{teacher = c(0,1,0,1,0,0,1),
#' lesson = c(1,3,4,5,2,3)}.
#' @return Returns a single-case data frame \code{scdf} suitable for all
#' functions of the \code{scan} package. Multiple data sets (e.g. from Multiple
#' Baseline Designs) can be listed.
#' @author Juergen Wilbert
#' @seealso \code{\link{longSCDF}}, \code{\link{readSC}}
#' \code{\link{writeSC}}
#' @examples
#' 
#' ## Scores on a letter naming task were collected on eleven days in a row. The intervention
#' ## started after the fifth measurement, so the first B phase measurement was 6 (B.start = 6).
#' klaas <- scdf(c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19), B.start = 6, name = "Klaas")
#' plot(klaas)
#' 
#' ## Unfortunately in a similar SCDR there were no data collected on days 3 and 9. Use NA to
#' ## pass them to the package.
#' emmi <- scdf(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19), 
#'              phase.design = c(A = 5, B = 6), name = "Emmi")
#' describeSC(emmi)
#' 
#' ## In a MBD over three persons, data were again collected eleven days in a row. Intervention
#' ## starting points differ between subjects as they were randomly assigned. The three SCDFs
#' ## are then combined in a list for further conjoined analyses.
#' charlotte <- scdf(c(5, 7, 10, 5, 12, 7, 10, 18, 15, 14, 19), B.start = 6)
#' theresa   <- scdf(c(3, 4, 3, 5, 7, 4, 7, 9, 8, 10, 12),      B.start = 5)
#' antonia   <- scdf(c(9, 8, 8, 7, 5, 7, 6, 14, 15, 12, 16),    B.start = 7)
#' mbd <- c(charlotte, theresa, antonia)
#' names(mbd) <- c("Charlotte", "Theresa", "Antonia")
#' overlapSC(mbd)
#' 
#' ## In a classroom-based intervention it was not possible to measure outcomes every day, but
#' ## only on schooldays. The sequence of measurements is passed to the package by using a
#' ## vector of measurement times.
#' frida <- scdf(c(3, 2, 4, 2, 2, 3, 5, 6, 8, 10, 8, 12, 14, 13, 12), B.start = 9,
#'     mt = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18))
#' summary(frida)
#' plot(frida)
#' describeSC(frida)
#' 
#' ## example with two independent variables and four phases
#' jim  <- scdf(zvt = c(47, 58, 76, 63, 71, 59, 64, 69, 72, 77, 76, 73), 
#'               d2 = c(131, 134, 141, 141, 140, 140, 138, 140, 141, 140, 138, 140), 
#'              phase.design = c(A1=3, B1=3, A2=3, B2=3), dvar = "zvt")
#' overlapSC(jim, phases = list(c("A1","A2"),c("B1","B2")))
scdf <- function (values = NULL, B.start = NULL, mt = NULL, phase = NULL, phase.design = NULL, name = NULL, dvar = "values", var.values = NULL, pvar = "phase", mvar = "mt", ...){
  add.var   <- data.frame(...)
  names.var <- names(add.var)
  
  if(is.null(values))
    data <- add.var
  
  if(!is.null(values)) {
    data <- data.frame(values = values)
    if(nrow(add.var) > 0)
      data <- cbind(data,add.var)
  }
  
  if(!is.null(phase))
    data$phase <- phase

  if(!is.null(mt))
    data$mt <- mt
  
  ### for backward campatibility
  if(!is.null(var.values)) {
    warning("'var.values' is deprecated. Please use 'dvar' instead.")
    dvar <- var.values
  }
  
  if (("MT" %in% names.var) && is.null(mt) && mvar == "mt") {
    warning("Variable is named 'MT' instead of 'mt'. Variable 'MT' renamed to 'mt'.")
    #mt <- add.var$MT
    #add.var <- add.var[ ,!names.var %in% "MT", drop = FALSE]
    #names.var <- names(add.var)
    names(data)[which(names(data) == "MT")] <- "mt"
  }
  ### END : for backward campatibility
  

  
  if(!(mvar %in% names(data)))
    data[,mvar] <- 1:nrow(data)
  
  if(!is.null(B.start)) {
    B.start <- match(B.start, data[,mvar])
    if(is.na(B.start))
      stop("No values provided at B.start.")
    phase.design <- c("A" = B.start - 1, "B" = nrow(data) - B.start + 1)
  }

  if(!is.null(phase.design))
    data[,pvar] <- rep(names(phase.design),phase.design)

  if(!(dvar %in% names(data)))
    stop("Independent variable not defined correctly!")
  if(!(pvar %in% names(data)))
    stop("Phase variable not defined correctly!")
  if(!(mvar %in% names(data)))
    stop("Measurement-time variable not defined correctly!")
  
  data <- list(data)
  attributes(data) <- .defaultAttributesSCDF() 
  
  attr(data, .opt$dv)    <- dvar
  attr(data, .opt$phase) <- pvar
  attr(data, .opt$mt)    <- mvar
  
  names(data) <- name
  data
}
