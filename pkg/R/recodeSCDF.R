##########
########## recodeSCDF
##########
########## 

recodeSCDF <- function(data, id = "case", phase = "phase", values = "values", mt = "mt", keep = NULL, phase.labels = NULL, sort.labels = FALSE) {
  
  if(is.na(mt))
    data$mt <- NA
  data <- data[,c(id, phase, values, mt, keep)]
  names(data)[1:4] <- c("case", "phase", "values", "mt")
  
  if(!sort.labels) 
    data$case <- factor(data$case, levels = unique(data$case))
  else
    data$case <- factor(data$case)
  
  data$phase <- factor(data$phase)
  
  if(!is.null(phase.labels))
    levels(data$phase) <- phase.labels
  
  lab <- levels(data$case)
  data <- split(data, data$case)
  data <- lapply(data, function(x) x[,-1])
  for(i in 1:length(data))
    row.names(data[[i]]) <- 1:nrow(data[[i]])
  names(data) <- lab
  cat("Found",length(data),"cases.\n")

  if(is.na(mt)) {
    cat("Measurement-times are missing. Standard times are assigned.\n")
    for(i in 1:length(data))
      data[[i]]$mt <- 1:nrow(data[[i]])
  }
  class(data) <- c("scdf","list")
  return(data)

}