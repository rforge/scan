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
  
  if(is.data.frame((object))) {
    object <- list(object)
  }
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
  min.row <- min(unlist(lapply(x[1:cases], nrow)))
  max.row <- max(unlist(lapply(x, nrow)))
  
  if(rows == "all") {
    long <- TRUE
  }
  if(!long) {
    if(min.row < rows) 
      rows <- min.row
    out <- lapply(x, function(x) x[1:rows,])
    if(cases > 1)
      out <- lapply(out, function(x) {x$"|" <- "|"; x})
    names <- lapply(out, names)
    out <- as.data.frame(out)
    names(out) <- unlist(names[1:cases])
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
  return(invisible(FALSE))
}


makeSCDF <- function (data, B.start = NULL, MT = NULL){
  warning("This function is deprecated. Please use the scdf function.\n\n")
  
  scdf(values = data, B.start = B.start, mt = MT)[[1]]
}
