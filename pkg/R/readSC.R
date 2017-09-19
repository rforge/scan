readSC.excel <- function(...) {
  readSC(..., type = "excel")
  
}


readSC <- function(filename = NULL, sep = ",", dec = ".", sort.labels = FALSE, type = "csv", ...) {
  if(is.null(filename)) {
    filename <- file.choose()
    cat("Import file",filename,"\n\n")
  }
  
  if(type == "csv")
    dat <- read.table(filename, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE, ...)
  if(type == "excel") {
    stop("Excel import currently not supported.")
    #dat <- as.data.frame(read_excel(filename, ...))
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
