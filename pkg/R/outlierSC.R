

outlierSC <- function(data, criteria = c("MAD", "3.5")){
  
  data.list <- .SCprepareData(data)
  if(!any(criteria[1] %in% c("MAD","Cook","SD","CI")))
    stop("Unknown criteria. Please check.")
  out <- list()
  
  N <- length(data.list)
  case.names <- names(data.list)
  dropped.mts <- list()
  dropped.n <- list()
  ci.matrix <- list()
  sd.matrix <- list()
  mad.matrix <- list()
  cook <- list()
  
  if(is.null(case.names))
    case.names <- paste("Case", 1:N, sep = "")
  
  for(i in 1:N) {
    data <- data.list[[i]]
    
    phases <- rle(as.character(data$phase))$value
    values <- lapply(phases, function(x) data$values[data$phase == x])
  
    if (criteria[1] == "CI") {
      cut.off <- as.numeric(criteria[2])
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase","m","se","lower", "upper")
      rownames(mat) <- names(values)
      filter <- c()
      fac <- qnorm((1-cut.off)/2, lower.tail = FALSE)
      
      for(p in 1:length(values)) {
        x <- values[[p]]
        mat[p,"m"] <- mean(x)
        mat[p,"se"] <- sd(x)/sqrt(length(x))
        mat[p,"lower"] <- mean(x) - fac * (sd(x)/sqrt(length(x)))
        mat[p,"upper"] <- mean(x) + fac * (sd(x)/sqrt(length(x)))
        filter <- c(filter, (x < mat[p,"lower"]) | (x > mat[p,"upper"]))
      }
      mat <- as.data.frame(mat)
      mat$phase <- phases
      ci.matrix[[i]] <- mat
    }
    
    if (criteria[1] == "MAD") {
      fac <- as.numeric(criteria[2])
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase","md","mad","lower", "upper")
      filter <- c()
      for(p in 1:length(values)) {
        x <- values[[p]]
        mat[p,"md"] <- median(x)
        mat[p,"mad"] <- mad(x,constant = 1)
        mat[p,"lower"] <- median(x) - fac * mad(x)
        mat[p,"upper"] <- median(x) + fac * mad(x)
        filter <- c(filter, (x < mat[p,"lower"]) | (x > mat[p,"upper"]))
      }
      mat <- as.data.frame(mat)
      mat$phase <- phases
      mad.matrix[[i]] <- mat
      
    }	
    if (criteria[1] == "SD") {
      SD <- as.numeric(criteria[2])
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase","m","sd","lower", "upper")
      filter <- c()
      for(p in 1:length(values)) {
        x <- values[[p]]
        mat[p,"m"] <- mean(x)
        mat[p,"sd"] <- sd(x)
        mat[p,"lower"] <- mean(x) - SD * sd(x)
        mat[p,"upper"] <- mean(x) + SD * sd(x)
        filter <- c(filter, (x < mat[p,"lower"]) | (x > mat[p,"upper"]))
      }
      mat <- as.data.frame(mat)
      mat$phase <- phases
      sd.matrix[[i]] <- mat
      
    }		
    if (criteria[1] == "Cook") {
      if(!identical(phases, c("A","B")))
         stop("Cook criteria only available for AB-designs.")
      A <- values[[1]]
      B <- values[[2]]
      if (criteria[2] == "4/n")
        cut.off <- 4/(length(A)+length(B))
      else
        cut.off <- as.numeric(criteria[2])

      n1 <- length(A)
      MT <- data$mt
      values <- data$values
      T <- MT[n1+1]
      D <- c(rep(0, length(A)), rep(1, length(B)))
      int <-  D * (MT - T)
      reg <- lm(values ~ 1 + MT + D + int)
      cd <- cooks.distance(reg)
      filter <- cd >= cut.off
      cook[[i]] <- data.frame(Cook = round(cd,2), MT = MT)
    }		
    
    #data.list[[i]][,4] <- filterAB
    #names(data.list[[i]])[4] <- "outlier"
    dropped.mts[[i]] <- data.list[[i]]$mt[filter]
    dropped.n[[i]] <- sum(filter)
    
    data.list[[i]] <- data.list[[i]][!filter,]
  }
  
  out$data <- data.list
  out$dropped.mt <- dropped.mts
  out$dropped.n <- dropped.n
  out$ci.matrix <- ci.matrix
  out$sd.matrix <- sd.matrix
  out$mad.matrix <- mad.matrix
  out$cook <- cook
  out$criteria <- criteria
  out$N <- N
  out$case.names <- case.names
  class(out) <- c("sc","outlier")
  out
}
