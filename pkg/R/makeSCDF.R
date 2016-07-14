
makeSCDF <- function (data, B.start = NULL, MT = NULL){

  
  #vector with B.start -> single data frame
  
  if (class(data) == "numeric" || class(data) == "integer") {
    if (is.null(MT))
      MT <- 1:length(data)
    B.start <- which(MT == B.start)
    D <- c(rep("A", B.start - 1), rep("B", length(data) - B.start + 1))
    data <- data.frame(phase = D, values = data, mt = MT)
  }
  
  #single data.frame -> list with single data frame
  #if (class(data) == "data.frame") {
  if(is.data.frame(data)) {
    if(ncol(data) == 2)
      data[3] <- 1:nrow(data)
    names(data) <- c("phase", "values", "mt")
    data <- list(data)
  }
  
  #list with vectors and vector with B.start -> list with data frames
  if(length(B.start) > 1) {
    if(length(data) != length(B.start) || mode(data) != "list")
      stop("Wrong data format. Format schould be a list of vectors and a vector of startpoints.")
    dat <- list()
    for(i in 1:length(data)) {
      if (is.null(MT))
        MT.tmp <- 1:length(D)
      else
        MT.tmp <- MT[[i]]
      B.start[i] <- which(MT.tmp == B.start[i])
      D <- c(rep("A", B.start[i] - 1), rep("B", length(data[[i]]) - B.start[i] + 1))
      dat[[i]] <- data.frame(phase = D, values = data[[i]], mt = MT.tmp)
    }
    data <- dat
  }
  
  if(class(data) != "list")
    stop("Wrong data format. Data have to be a numeric vector, a data.frame, or a list.")
  
  
  for(i in 1:length(data))
    if(ncol(data[[i]]) == 2) data[[i]]$mt <- 1:nrow(data[[i]])
    
    if(length(data) == 1)
      return(data[[1]])
    return(data)

}
