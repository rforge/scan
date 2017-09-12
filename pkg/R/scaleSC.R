
scaleSC <- function(data, center = TRUE, scale = FALSE, m = 0, sd = 1, grand = TRUE, var = "values") {
  
  data <- .SCprepareData(data)
  N    <- length(data)
  
  if(grand) {
    means <- c()
    sds <- c()
    for(i in 1:length(var)) {
      means <- c(means,mean(unlist(lapply(data, function(x) x[,var[i]])), na.rm = TRUE))
      sds   <- c(sds,    sd(unlist(lapply(data, function(x) x[,var[i]])), na.rm = TRUE))
    }
    
    for(case in 1:N) {
      for(i in 1:length(var)) {
        if(center && scale)
          data[[case]][,var[i]] <- (data[[case]][,var[i]] - means[i]) / sds[i] * sd + m
        if(center && !scale)
          data[[case]][,var[i]] <- (data[[case]][,var[i]] - means[i]) + m
        if(!center && scale)
          data[[case]][,var[i]] <- ((data[[case]][,var[i]] - means[i]) / sds[i] * sd) + means[i]
      }
    }
  }
  
  if(!grand) {
    for(case in 1:N) {
      for(i in 1:length(var)) {
        mCase  <- mean(data[[case]][,var[i]], na.rm = TRUE)
        sdCase <-   sd(data[[case]][,var[i]], na.rm = TRUE)
        if(center && scale)
          data[[case]][,var[i]] <- (data[[case]][,var[i]] - mCase) / sdCase[i] * sd + m
        if(center && !scale)
          data[[case]][,var[i]] <- (data[[case]][,var[i]] - mCase) + m
        if(!center && scale)
          data[[case]][,var[i]] <- ((data[[case]][,var[i]] - mCase) / sdCase * sd) + mCase
      }
    }
  }

  data
}
