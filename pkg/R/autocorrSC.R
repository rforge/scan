
autocorrSC <- function(data, lag.max = 3) {
  data <- .SCprepareData(data)
  
  N <- length(data)
  case.names <- names(data)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  VAR <- paste0("lag_",1:lag.max)
  
  design <- rle(as.character(data[[1]]$phase))$values
  
  while(any(duplicated(design))) {
    design[anyDuplicated(design)] <- paste0(design[anyDuplicated(design)],".phase",anyDuplicated(design))
  }
  
  
  ac <- data.frame(case = rep(case.names, each = length(design)+1), phase = rep(c(design,"all"), N))
  ac[,VAR] <- NA
  
  
  for(case in 1:N) {
    phases <- rle(as.character(data[[case]]$phase))
    phases$start <- c(1,cumsum(phases$lengths)+1)[1:length(phases$lengths)]
    phases$stop <- cumsum(phases$lengths)
    class(phases) <- "list"
    
    for(phase in 1:length(design)) {
      y <- data[[case]]$values[phases$start[phase]:phases$stop[phase]]
      if(length(y)-1 < lag.max) lag <- length(y)-1 else lag <- lag.max
      
      ac[(case-1)*(length(design)+1)+phase,VAR[1:lag]] <- acf(y, lag.max = lag, plot = FALSE)$acf[-1]
    }
    y <- data[[case]]$values
    if(length(y)-1 < lag.max) lag <- length(y)-1 else lag <- lag.max
    
    ac[(case-1)*(length(design)+1)+(length(design)+1),VAR[1:lag]] <- acf(y, lag.max = lag, plot = FALSE)$acf[-1]
    
  }
  #for(i in (0:(N-1)*3)) {
    #data <- data.list[[(i/3+1)]]
    #A <- data[,2][data[,1] == "A"]
    #B <- data[,2][data[,1] == "B"]
    #if(length(A)-1 < lag.max) lagA <- length(A)-1 else lagA <- lag.max
    #if(length(B)-1 < lag.max) lagB <- length(B)-1 else lagB <- lag.max
    #if(length(c(A,B))-1 < lag.max) lagAB <- length(c(A,B))-1 else lagAB <- lag.max
    
    
    #ac[i+1,VAR[1:lagA]] <- acf(A, lag.max = lagA, plot = FALSE)$acf[-1]
    #ac[i+2,VAR[1:lagB]] <- acf(B, lag.max = lagB, plot = FALSE)$acf[-1]
    #ac[i+3,VAR[1:lagAB]] <- acf(c(A,B), lag.max = lagAB, plot = FALSE)$acf[-1]
   #}

  out <- list(autocorr = ac)
  class(out) <- c("sc","autocorr")
  out
}
