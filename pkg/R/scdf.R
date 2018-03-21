methods::setOldClass(c("scdf", "list"))

c.scdf <- function(...) {
  data <- unlist(list(...), recursive = FALSE)
  
  attributes(data) <- .defaultAttributesSCDF(attributes(data)) 
  return(data)
}

`$.scdf`<- function(x, i) {
  if (is.character(i) && !(i %in% names(x))) {
    warning("Unknown case: '", i, "'.")
  }
  ATTRIBUTES <- attributes(x)
  
  out <- x[i]
  attributes(out)[c("phase","values", "mt")] <- ATTRIBUTES[c("phase","values", "mt")]
  class(out) <- c("scdf","list")
  out
}

`[.scdf`<- function(x, i) {
  ATTRIBUTES <- attributes(x)
  
  class(x) <- "list"
  out <- x[i]
  attributes(out)[c("phase","values", "mt")] <- ATTRIBUTES[c("phase","values", "mt")]
  class(out) <- c("scdf","list")
  out
}

summary.scdf <- function(object, var.names = FALSE, ...) {
  cat("#A single-case data frame with", length(object),"cases\n\n")
  designs <- lapply(object, function(x) paste0(rle(as.character(x$phase))$values,collapse = ""))
  rows <- lapply(object, nrow)
  out <- data.frame(Measurements = unlist(rows), Design = unlist(designs))
  if(!is.null(names(object)))
    row.names(out) <- names(object)
  if(var.names) {
    cat(paste0("Variable name of values: ",attr(object,"values")),"\n")
    cat(paste0("Variable name of phase: ",attr(object,"phase")),"\n")
    cat(paste0("Variable name of mt: ",attr(object,"mt")),"\n")
    cat("\n")
  }
  print(out)
  if(!is.null(attr(object,"info"))) {
    cat("\nNote: ",attr(object,"info"))
    
  }
  if(!is.null(attr(object,"author"))) {
    cat("\nAuthor of data: ",attr(object,"author"),"\n")
    
  }
  
  
}

as.scdf <- function(object) {
  
  if(is.data.frame((object)))
    object <- list(object)

  if(!is.list(object))
    stop("Object must be a data.frame or a list of data.frames.")
  
  VARS <- c("phase","values","mt")
  if(!all(unlist(lapply(object, function(x) all(VARS %in% names(x))))))
    stop("All data.frames must contain the variables phase, values, and mt.")
  
  attributes(object) <- .defaultAttributesSCDF(attributes(object)) 
  object
  
}



print.scdf <- function(x, cases = 3, rows = 15, row.names = FALSE, long = FALSE, ...) {
  N <- length(x)
  if(is.null(names(x)))
    names(x) <- paste("Case",1:N, sep = "")
  
  if(cases == "all")
    cases <- N
  if(cases > N) cases <- N
  if(N == 1)
    cat("#A single-case data frame with one case\n\n")
  if(N > 1)
    cat("#A single-case data frame with",N,"cases\n\n")

  x <- x[1:cases]
  for(i in 1:length(x)) {
    names(x[[i]])[1] <- paste0(names(x)[i],": ",names(x[[i]])[1])
  }
  max.row <- max(unlist(lapply(x, nrow)))
  
  for(i in 1:cases){
    n.row <- nrow(x[[i]])
    x[[i]][,1] <- as.character(x[[i]][,1])
    if(n.row < max.row) 
      x[[i]][(n.row + 1):max.row, names(x[[i]])] <- ""
  }
  
  #min.row <- min(unlist(lapply(x[1:cases], nrow)))
  
  if(rows == "all") {
    long <- TRUE
  }
  if(!long) {
    if(max.row < rows) 
      rows <- max.row
    out <- lapply(x, function(x) x[1:rows,])
    if(cases > 1)
      out <- lapply(out, function(x) {x$"|" <- "|"; x})
    names <- lapply(out, names)
    out <- as.data.frame(out)
    names(out) <- unlist(names[1:cases])
    
    # name <- " "
    # sep  <- "-"
    # for(i in 1:cases) {
    #   fx <- format(x[[i]])  
    #   chars <- 0
    #   j <- 1
    #   for(j in 1:ncol(fx)) {
    #     width <- max(nchar(fx[[j]]))
    #     if(width < nchar(names(fx)[j])) width <- nchar(names(fx)[j])
    #     chars <- chars + width + 1
    #   }
    #   chars <- chars - 1
    #   name <- paste0(name, format(names(x)[i], width = chars)," | ")
    #   tmp.sep <- paste0(rep("-",chars), collapse = "")
    #   sep <- paste0(sep, tmp.sep, "-|-")
    # }
    # cat(sep)
    # cat("\n")
    # cat(name)
    # cat("\n")
    # cat(sep)
    # cat("\n")
    
    print(out, row.names = row.names, ...)
  }
  if(long) {
    for(case in 1:length(x)) {
      print(x[[case]], row.names = row.names, ...)
      cat("\n")
    }
  }
  
  if(max.row > rows)
    cat("# ... up to",max.row-rows,"more rows\n")
  if(N > cases) {
    if((N-cases) > 1) 
      cat("# ",N-cases,"more cases\n")
    if((N-cases) == 1) 
      cat("# One more case\n")
    
  }
  
}


#scdf <- function (data, B.start = NULL, MT = NULL, phase.length = NULL, pvar = NULL, name = NULL){


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
#' @aliases scdf scdf-class summary.scdf print.scdf as.scdf c.scdf checkSCDF
#' makeSCDF
#' @param data A vector containing measurement values of the target variable.
#' @param B.start The first measurement of phase B (simple coding if design is
#' strictly AB).
#' @param MT A vector defining measurement times. Default is \code{MT =
#' (1,2,3,...,n)}.
#' @param phase.design A vector defining the length and label of each phase.
#' E.g., \code{phase.length = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)}.
#' @param name A name for the case.
#' @param phase -
#' @param values -
#' @param mt -
#' @param object -
#' @param x -
#' @param cases -
#' @param rows -
#' @param row.names -
#' @param long -
#' @param var.names -
#' @param ...  Additional variables. E.g., \code{teacher = c(0,1,0,1,0,0,1),
#' lesson = c(1,3,4,5,2,3)}.
#' @return Returns a single-case data frame \code{scdf} suitable for all
#' functions of the \code{scan} package. Multiple data sets (e.g. from Multiple
#' Baseline Designs) can be listed.
#' @author Juergen Wilbert
#' @seealso \code{\link{longSCDF}}, \code{\link{makesingleSC}},
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
#' emmi <- scdf(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19), B.start = 6, name = "Emmi")
#' describeSC(emmi)
#' 
#' ## In a MBD over three persons, data were again collected eleven days in a row. Intervention
#' ## starting points differ between subjects as they were randomly assigned. The three SCDFs
#' ## are then combined in a list for further conjoined analyses.
#' charlotte <- scdf(c(5, 7, 10, 5, 12, 7, 10, 18, 15, 14, 19), B.start = 6)
#' theresa <- scdf(c(3, 4, 3, 5, 7, 4, 7, 9, 8, 10, 12),B.start = 5)
#' antonia <- scdf(c(9, 8, 8, 7, 5, 7, 6, 14, 15, 12, 16), B.start = 7)
#' mbd <- c(charlotte, theresa, antonia)
#' names(mbd) <- c("Charlotte", "Theresa", "Antonia")
#' overlapSC(mbd)
#' 
#' ## In a classroom-based intervention it was not possible to measure outcomes every day, but
#' ## only on schooldays. The sequence of measurements is passed to the package by using a
#' ## vector of measurement times.
#' frida <- scdf(c(3, 2, 4, 2, 2, 3, 5, 6, 8, 10, 8, 12, 14, 13, 12), B.start = 9,
#'     MT = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18))
#' summary(frida)
#' plot(frida)
#' describeSC(frida)
#' 
#' 
scdf <- function (values = NULL, B.start = NULL, mt = NULL, phase = NULL, phase.design = NULL, name = NULL, ...){
  add.var <- data.frame(...)
  names.var <- names(add.var)
  if ("MT" %in% names.var && is.null(mt)) {
    warning("Variable is named 'MT' instead of 'mt'. Variable 'MT' renamed to 'mt'.")
    mt <- add.var$MT
    add.var <- add.var[ ,!names.var %in% "MT", drop = FALSE]
    names.var <- names(add.var)
  }
  
  if ("data" %in% names.var && is.null(values)) {
    warning("Variable is named 'data' instead of 'values'. Variable 'data' renamed to 'values'.")
    values <- add.var$data
    add.var <- add.var[ ,!names.var %in% "data"]
  }
  
  if (is.null(mt))
    mt <- 1:length(values)
  if(!is.null(B.start)) {
    B.start <- match(B.start, mt) #which(mt == B.start)
    if(is.na(B.start))
      stop("No values provided at B.start.")
    phase.design <- c("A" = B.start-1, "B" = length(values) - B.start + 1)
  }
  
  if(is.null(phase))
     phase <- rep(names(phase.design),phase.design)
  
  data <- data.frame(phase = phase, values = values, mt = mt)
 
  if(nrow(add.var) > 0)
     data <- cbind(data,add.var)
  data <- list(data)
  attributes(data) <- .defaultAttributesSCDF() 
  names(data) <- name
  data
}

checkSCDF <- function(data) {
  cat("Checking object ...\n\n")
  if(!identical(class(data),c("scdf","list")))
    cat("Object is not of class 'scdf'.\n")
  if(!("list" %in% class(data))) {
    cat("Object is not of class 'list'.\n")
    return(invisible(FALSE))
  }
  if(!all(unlist(lapply(data, function(x) is.data.frame(x))))) {
    cat("Not all list-elements are data frames.\n")
    return(invisible(FALSE))
  }
  if(!all(unlist(lapply(data, function(x) {c("phase")%in%names(x)})))) {
    cat("Not all dataframes have a phase column.\n")
    return(invisible(FALSE))
  }
  if(!all(unlist(lapply(data, function(x) {c("values")%in%names(x)})))) {
    cat("Not all dataframes have a values column.\n")
    return(invisible(FALSE))
  }
  if(!all(unlist(lapply(data, function(x) {c("mt")%in%names(x)}))))
    cat("Note: Not all dataframes have an 'mt' column.\n")
  phases <- rle(as.character(data[[1]]$phase))$values
  if(!all(unlist(lapply(data, function(x) identical(rle(as.character(x$phase))$values, phases)))))
    cat("Warning: Phases are not identical for all cases.\n")
  cat("Done!\n")
  return(invisible(FALSE))
}


makeSCDF <- function (data, B.start = NULL, MT = NULL){
  warning("This function is deprecated. Please use the scdf function.\n\n")
  
  scdf(values = data, B.start = B.start, mt = MT)[[1]]
}
