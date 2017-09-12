

newrankSC <- function(data) {
  data.list <- .SCprepareData(data)
  N <- length(data.list)

  for(i in 1:N) {
    data.list[[i]]$values <- rank(data.list[[i]]$values)
  }
  class(data.list) <- c("scdf","list")
  
  data.list
}

