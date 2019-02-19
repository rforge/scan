methods::setOldClass(c("scdf", "list"))

c.scdf <- function(...) {
  ATTRIBUTES <- attributes(..1)
  
  LEN <- length(list(...))
  NAMES <- c()
  for(i in 1:LEN)
    NAMES <- c(NAMES, names(...elt(i)))

  data <- unlist(list(...), recursive = FALSE)

  attributes(data) <- .defaultAttributesSCDF()

  if(!is.null(ATTRIBUTES$var.values))
    attr(data, .opt$dv) <- ATTRIBUTES[[.opt$dv]]
  if(!is.null(ATTRIBUTES$var.phase))
    attr(data, .opt$phase) <- ATTRIBUTES[[.opt$phase]]
  if(!is.null(ATTRIBUTES$var.mt))
    attr(data, .opt$mt) <- ATTRIBUTES[[.opt$mt]]
  names(data) <- NAMES
  return(data)
}

`$.scdf`<- function(x, i) {
  if (is.character(i) && !(i %in% names(x))) {
    warning("Unknown case: '", i, "'.")
  }
  ATTRIBUTES <- attributes(x)
  
  out <- x[i]
  attributes(out)[c(.opt$phase,.opt$dv, .opt$mt)] <- ATTRIBUTES[c(.opt$phase,.opt$dv, .opt$mt)]
  class(out) <- c("scdf","list")
  out
}

`[.scdf`<- function(x, i) {
  ATTRIBUTES <- attributes(x)
  
  class(x) <- "list"
  out <- x[i]
  attributes(out)[c(.opt$phase,.opt$dv, .opt$mt)] <- ATTRIBUTES[c(.opt$phase,.opt$dv, .opt$mt)]
  class(out) <- c("scdf","list")
  out
}

summary.scdf <- function(object, var.names = TRUE, ...) {
  #object <- .SCprepareData(object)
  if(length(object)>1)
     cat("#A single-case data frame with", length(object),"cases\n\n")
  else
    cat("#A single-case data frame with one case\n\n")
  
  designs <- lapply(object, function(x) paste0(rle(as.character(x[,attr(object,.opt$phase)]))$values,collapse = " "))
  rows <- lapply(object, nrow)
  out <- data.frame(Measurements = unlist(rows), Design = unlist(designs))
  if(!is.null(names(object)))
    row.names(out) <- names(object)

  print(out)
  cat("\n", sep = "")
  if(var.names) {
    cat("Variable names:\n")
    name.tmp <- names(object[[1]])
    n.tmp <- which(name.tmp == attr(object,.opt$dv))
    name.tmp[n.tmp] <- paste0(name.tmp[n.tmp], " <dependend variable>")
    n.tmp <- which(name.tmp == attr(object,.opt$phase))
    name.tmp[n.tmp] <- paste0(name.tmp[n.tmp], " <phase variable>")
    n.tmp <- which(name.tmp == attr(object,.opt$mt))
    name.tmp[n.tmp] <- paste0(name.tmp[n.tmp], " <measurement-time variable>")
    cat(name.tmp,sep = "\n")
    #cat(paste0("iv: ",attr(object,.opt$dv)),"\n")
    #cat(paste0("phase:  ",attr(object,.opt$phase)),"\n")
    #cat(paste0("mt:     ",attr(object,.opt$mt)),"\n")
    cat("\n")
  }
  
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
