#' Print an scdf
#'
#' @param x An scdf object
#' @param cases Number of cases to be printed.
#' @param rows Number of rows to be printed. 
#' @param cols Columns to be printed.
#' @param row.names Logical. If TRUE row names are printed.
#' @param long Logical. If TRUE cases are printed in one by a time.
#' @param ... Further arguments passed to the print function. 
#'
#' @export
#'
print.scdf <- function(x, cases = 3, rows = 15, cols = "all", row.names = FALSE, long = FALSE, ...) {
  N <- length(x)
  
  if(is.null(names(x)))
    names(x) <- paste0("Case",1:N)
  nonames <- which(is.na(names(x)))
  names(x)[nonames] <- paste0("Case",nonames)
  
  
  if(cases == "all")
    cases <- N
  if(cases > N) cases <- N
  if(N == 1)
    cat("#A single-case data frame with one case\n\n")
  if(N > 1)
    cat("#A single-case data frame with",N,"cases\n\n")
  
  if(cols[1] == "main")
    cols = c(attr(x, "var.phase"), attr(x, "var.values"), attr(x, "var.mt"))

  for(i in 1:N) {
    MAXCOLS <- ncol(x[[i]])
    COLS <- cols
    if(cols[1] == "all")
      COLS <- 1:MAXCOLS
    x[[i]] <- x[[i]][, COLS]
  }
  
  
  for(i in 1:N)
    names(x[[i]])[1] <- paste0(names(x)[i],": ",names(x[[i]])[1])

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
    out <- lapply(x[1:cases], function(x) x[1:rows,])
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
    for(case in 1:N) {
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
