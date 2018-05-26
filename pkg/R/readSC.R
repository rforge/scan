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
#' @param \dots Further arguments passed to the \code{\link{read.table}}
#' command.
#' @return Returns a single-case data frame. See \code{\link{scdf}} to learn
#' about the format of these data frames.
#' @author Juergen Wilbert
#' @seealso \code{\link{read.table}}, \code{\link{writeSC}}, \code{\link{scdf}}
#' @keywords manip
#' @examples
#' 
#' ## Read SC-data from a file named "study1.csv" in your working directory
#' # study1 <- readSC("study1.csv")
#' 
#' ## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
#' # study2 <- readSC("study2.csv", sep = ";", dec = ",")
#' 
readSC <- function(filename = NULL, sep = ",", dec = ".", sort.labels = FALSE, type = "csv", ...) {
  if(is.null(filename)) {
    filename <- file.choose()
    cat("Import file",filename,"\n\n")
  }
  
  if(type == "csv")
    dat <- read.table(filename, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE, ...)
  if(type == "excel") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package readxl needed for this function to work. Please install it.",
           call. = FALSE)
    }
    #stop("Excel import currently not supported.")
    dat <- as.data.frame(readxl::read_excel(filename, ...))
  }
  
  columns <- ncol(dat)
  names(dat) <- c("case", "phase", "values", "mt")[1:columns]
  if(!sort.labels) 
    dat$case <- factor(dat$case, levels = unique(dat$case))
  else
    dat$case <- factor(dat$case)
  
  dat$phase <- factor(dat$phase)
  
  lab <- levels(dat$case)
  dat <- split(dat, dat$case)
  dat <- lapply(dat, function(x) x[,2:columns])
  for(i in 1:length(dat))
    row.names(dat[[i]]) <- 1:nrow(dat[[i]])
  names(dat) <- lab
  cat("Imported",length(dat),"cases.\n")
  if(columns == 3) {
    cat("Measurement-times are missing. Standard times were assigned.\n")
    dat <- .SCprepareData(dat)
  }
  class(dat) <- c("scdf","list")
  return(dat)
}


readSC.excel <- function(...) {
  readSC(..., type = "excel")
  
}

