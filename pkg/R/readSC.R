#' Read single-case data from files
#' 
#' Use the \code{readSC} function to import single-case data from structured
#' .csv or the \code{readSC.excel} function for importing excel files.
#' 
#' 
#' @aliases readSC readSC.excel
#' @param filename A character string defining the file to be imported (e.g.
#' \code{"SC_Anita.csv"}. If filename is left empty a dialog box for choosing
#' will be opened.
#' @param sep The field separator string. Values within rows have to be
#' separated by this string. Default is \code{sep = ","}.
#' @param dec The string used for decimal points in the file. Must be a single
#' character. Default is \code{dec = "."}
#' @param sort.labels If set TRUE, the resulting list is sorted by label names
#' (alphabetically increasing).
#' @param type Format of the file to be imported. Either "csv" or "excel" is
#' possible.
#' @param var.case Sets the variable name of the "case" variable. Deafults to \code{"case"}.
#' @param var.phase Sets the variable name of the "phase" variable. Deafults to \code{"phase"}.
#' @param var.values Sets the variable name of the "values" variable. Deafults to \code{"values"}.
#' @param var.mt Sets the variable name of the "mt" variable. Deafults to \code{"mt"}.
#' @param phase.names A character vector with phase names. Deafults to the phase names provided in the phase variable.
#' @param \dots Further arguments passed to the \code{\link{read.table}}
#' command.
#' @return Returns a single-case data frame. See \code{\link{scdf}} to learn
#' about the format of these data frames.
#' @author Juergen Wilbert
#' @seealso \code{\link{read.table}}, \code{\link{writeSC}}, \code{\link{scdf}}, \code{\link{readRDS}}
#' @keywords manip
#' @examples
#' 
#' ## Read SC-data from a file named "study1.csv" in your working directory
#' # study1 <- readSC("study1.csv")
#' 
#' ## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
#' # study2 <- readSC("study2.csv", sep = ";", dec = ",")
#' 
readSC <- function(filename = NULL, data = NULL, sep = ",", dec = ".", sort.labels = FALSE, var.case = "case", var.phase = "phase", var.values = "values", var.mt = "mt", phase.names = NULL, type = "csv", ...) {
  if(is.null(filename) && is.null(data)) {
    filename <- file.choose()
    cat("Import file",filename,"\n\n")
  }
  if(!is.null(data)) {
    type <- "data"
    dat <- as.data.frame(data)
  }
  
  if(type == "csv")
    dat <- read.table(filename, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE,...)
  if(type == "excel") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package readxl needed for this function to work. Please install it.",
           call. = FALSE)
    }
    #stop("Excel import currently not supported.")
    dat <- as.data.frame(readxl::read_excel(filename, ...))
  }

  VARS <- c(var.case, var.phase, var.values, var.mt)
  columns <- ncol(dat)
 
  pos <- match(VARS, names(dat))
  pos.rest <- which(!(1:columns %in% pos))
  dat <- dat[, c(pos, pos.rest)]

  if(!sort.labels) 
    dat[, var.case] <- factor(dat[, var.case], levels = unique(dat[, var.case]))
  else
    dat[, var.case] <- factor(dat[, var.case])
  
  dat[,var.phase] <- factor(dat[, var.phase])
  
  if(!is.null(phase.names))
    levels(dat[,var.phase]) <- phase.names
  
  lab <- levels(dat[, var.case])
  dat <- split(dat, dat[, var.case])
  dat <- lapply(dat, function(x) x[,2:columns])
  for(i in 1:length(dat))
    row.names(dat[[i]]) <- 1:nrow(dat[[i]])
  names(dat) <- lab
  cat("Imported",length(dat),"cases.\n")
  #if(columns == 3) {
  #  cat("Measurement-times are missing. Standard times were assigned.\n")
  #  dat <- .SCprepareData(dat)
  #}
  class(dat) <- c("scdf","list")
  attr(dat, "var.phase")  <- var.phase
  attr(dat, "var.values") <- var.values
  attr(dat, "var.mt")     <- var.mt
  
  
  return(dat)
}


readSC.excel <- function(...) {
  readSC(..., type = "excel")
  
}

