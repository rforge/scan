.kendall <- function(x,y) {
  
  out <- list()
  dat <- data.frame(cbind(x,y))
  dat <- dat[order(dat$x),]
  C <- 0
  D <- 0
  N <- nrow(dat)
  for(i in 1:(N-1)) {
    
    C <- C + sum( dat$y[(i+1):N] > dat$y[i] & dat$x[(i+1):N] > dat$x[i])
    D <- D + sum( dat$y[(i+1):N] < dat$y[i] & dat$x[(i+1):N] > dat$x[i])
    
  }
  
  tie.x <- rle(sort(x))$lengths
  tie.y <- rle(sort(y))$lengths
  
  ti <- sum(sapply(tie.x,function(x) (x*(x-1)/2)))
  ui <- sum(sapply(tie.y,function(x) (x*(x-1)/2)))
  
  S <- C-D
  n0 <- N*(N-1)/2
  out$N <- N
  out$n0 <- n0
  out$ti <- ti
  out$ui <- ui
  out$.C <- C
  out$.D <- D
  out$S <- S
  out$tau <- S/n0
  out$tau.b <- S / sqrt( (n0-ti)*(n0-ui) )
  #out$se <- sqrt( (2*N+5)/n0) / 3
  #out$z  <- 3*S / sqrt( n0*(2*N+5)/2 )
  #out$z <- out$tau.b / out$se
  #out$p  <- (1-pnorm(out$z, lower = FALSE)) *2
  out$D <- out$S / out$tau.b
  v0 <- N *(N-1) * (2*N+5)
  vt <- sum(sapply(tie.x,function(x) (x*(x-1))*(2*x+5)))
  vu <- sum(sapply(tie.y,function(x) (x*(x-1))*(2*x+5)))
  v1 <- sum(sapply(tie.x,function(x) (x*(x-1)))) * sum(sapply(tie.y,function(x) (x*(x-1))))
  v2 <- sum(sapply(tie.x,function(x) (x*(x-1))*(x-2))) * sum(sapply(tie.y,function(x) (x*(x-1))*(x-2)))
  
  out$varS <- (v0 - vt - vu)/18 + (v1/(2*N*(N-1))) + (v2 /(9*N*(N-1)*(N-2)))
  
  out$sdS <- sqrt(out$varS)
  out$se <- out$sdS/out$D
  out$z <- out$tau.b / out$se
  out$p  <- (1-pnorm(out$z, lower.tail = FALSE)) *2
  out
}




#' Tau-U for single-case data
#' 
#' This function calculates indices of the Tau-U family as proposed by Parker
#' et al. (2011).
#' 
#' 
#' @param data A single-case data frame.
#' @param ties.method Defines how to handle ties. \code{"omit"} (default) excludes all
#' ties from the calculation. \code{"positive"} counts all ties as positive
#' comparisons, while \code{"negative"} counts them as negative comparisons.
#' @param phases hases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second to the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @param method \code{"complete"} (default) or \code{"parker"}. Teh latter calculates the number of possible pairs as described in Parler et al. (2011) which might lead to tau-U values greater than 1.
#' @return \item{table}{A data frame containing statistics from the Tau-U
#' family, including: Pairs, positive and negative comparisons, S, and Tau}
#' \item{matrix}{The matrix of comparisons used for calculating the
#' statistics.} \item{tau_u}{Tau-U value.}
#' @author Juergen Wilbert
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B.
#' (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
#' \emph{Behavior Therapy, 42}, 284-299.
#' @examples
#' 
#' ## Calculate tau-U for the example from Parker et al. (2011)
#' bob <- scdf(c(2, 3, 5, 3, 4, 5, 5, 7, 6), B.start = 5)
#' tauUSC(bob)
#' 
#' ## Calculate tau-U with ties counted as positive
#' tauUSC(Grosche2011$Eva, ties.method = "positive")
#' 
#' ## Request tau-U for all single-cases fom the Grosche2011 data
#' tauUSC(Grosche2011)
#' 
tauUSC <- function (data, ties.method = "omit", method = "complete", phases = c("A","B")) {
  data <- .SCprepareData(data)
  data <- .keepphasesSC(data, phases = phases)$data
  
  N <- length(data)
  ret <- list(
    table = list(), 
    matrix = list(), 
    tau_u = list(),
    N = N,
    method = method
  )
  ROWS <- c("A vs. B","Trend A","Trend B","Trend B - Trend A","A vs. B - Trend A","A vs. B + Trend B", "A vs. B + Trend B - Trend A")
  COLS <- c("pairs", "pos", "neg","ties","S","D","Tau","Tau.b","SD","VAR","Z","p")
  for(i in 1:N) {
    out <- matrix(NA,length(ROWS),length(COLS), dimnames= list(ROWS,COLS))
    out <- as.data.frame(out)
    
    A <- data[[i]][data[[i]]$phase == "A","values"]
    B <- data[[i]][data[[i]]$phase == "B","values"]  
    AB <- c(A,B)
    nA <- length(A)
    nB <- length(B)
    nAB <- nA+nB
    
    tau_m <- matrix(NA, nrow = nAB, ncol = nAB, dimnames = (list(AB,AB)))
    tmp <- t(sapply(AB,function(x) x > AB))
    tau_m[tmp] <- "-"
    tmp <- t(sapply(AB,function(x) x < AB))
    tau_m[tmp] <- "+"
    tmp <- t(sapply(AB,function(x) x == AB))
    tau_m[tmp] <- "T"
    
    diag(tau_m) <- 0
    tau_m[lower.tri(tau_m)] <- ""
    
    pos.s <- c("+")
    neg.s <- c("-")
    tie.s <- c("T")
    if(ties.method == "positive") 
      pos.s <- c("+","T")
    if(ties.method == "negative") 
      neg.s <- c("-","T")
 
    AvBm <- tau_m[1:nA,(nA+1):nAB]
    AvBpos <- sum(AvBm%in%pos.s)
    AvBneg <- sum(AvBm%in%neg.s)
    AvBtie <- sum(AvBm%in%tie.s)
    
    AvAm <- tau_m[1:nA,1:nA]
    AvApos <- sum(AvAm%in%pos.s)
    AvAneg <- sum(AvAm%in%neg.s)
    AvAtie <- sum(AvAm%in%tie.s)
    
    BvBm <- tau_m[(nA+1):nAB,(nA+1):nAB]
    BvBpos <- sum(BvBm%in%pos.s)
    BvBneg <- sum(BvBm%in%neg.s)
    BvBtie <- sum(BvBm%in%tie.s)
    
    AvBKen      <- .kendall(AB, c(rep(0,nA),rep(1,nB)))
    AvAKen      <- .kendall(A, 1:nA)
    BvBKen      <- .kendall(B, 1:nB)
    BvB_AKen    <- .kendall(c(A,B), c(nA:1,1:nB))
    AvB_B_AKen  <- .kendall(c(A, B),c(nA:1,(nA + 1):nAB))
    AvB_AKen    <- .kendall(c(A, B),c(nA:1,rep(nA+1,nB)))
    AvB_BKen    <- .kendall(c(A, B),c(rep(0, nA),(nA + 1):nAB))
    
    if(method == "parker") {
      out$pairs <- c(
        nA*nB,                                   # A vs. B
        (nA*(nA-1))/2,                           # A vs. A
        (nB*(nB-1))/2,                           # B vs. B
        (nA*(nA-1))/2 + (nB*(nB-1))/2,           # A vs. A - B vs. B
        #(nAB*(nAB-1))/2,                        
        nA*nB + (nA*(nA-1))/2,                   # A vs. B - A vs. A 
        nA*nB + (nB*(nB-1))/2,                   # A vs. B + B vs. B
        (nAB*(nAB-1))/2 #nA*nB + (nB*(nB-1))/2   # A vs. B + B vs. B - A vs. A
      )
    }
    
    if(method == "complete") {
      out$pairs <- c(
        nA*nB,                                    #A vs. B
        (nA*(nA-1))/2,                            #A vs. A
        (nB*(nB-1))/2,                            
        (nB*(nB-1))/2 + (nA*(nA-1))/2,
        #(nAB*(nAB-1))/2, 
        nA*nB + (nA*(nA-1))/2,
        nA*nB + (nB*(nB-1))/2, 
        nA*nB + (nA*(nA-1))/2 + (nB*(nB-1))/2
      )
    }
    
    out$pos <- c(
      AvBpos,
      AvApos,
      BvBpos, 
      BvBpos+AvAneg, 
      #AvApos+BvBpos+AvBpos, 
      AvBpos+AvAneg, 
      AvBpos+BvBpos, 
      AvBpos+BvBpos+AvAneg
    )
    
    out$neg <- c(
      AvBneg,
      AvAneg,
      BvBneg, 
      BvBneg+AvApos, 
      #AvAneg+BvBneg+AvBneg, 
      AvBneg+AvApos, 
      AvBneg+BvBneg, 
      AvBneg+BvBneg+AvApos
    )
    
    out$ties <- c(
      AvBtie,
      AvAtie,
      BvBtie, 
      BvBtie+AvAtie, 
      #AvAtie+BvBtie+AvBtie, 
      AvBtie+AvAtie, 
      AvBtie+BvBtie, 
      AvBtie+BvBtie+AvAtie
    )
    
    
    out$S <- out$pos-out$neg
    
    out$D <- c(
      out$pairs[1]-out$ties[1]/2,
      AvAKen$D,
      BvBKen$D,
      BvB_AKen$D,
      #NA,
      AvB_AKen$D,
      AvB_BKen$D,
      AvB_B_AKen$D
    )
    
    
    out$Tau <- out$S / out$pairs
    out$Tau.b <- out$S / out$D
    out$SD <- c(
      sqrt((nA*nB)*(nA+nB+1)/12)*2,
      sqrt(.kendall(sample(nA),1:nA)$varS),
      sqrt(.kendall(sample(nB),1:nB)$varS),
      sqrt(.kendall(sample(nA+nB), c(nA:1,1:nB))$varS),
      #NA,
      sqrt(AvB_AKen$varS),
      sqrt(AvB_BKen$varS),
      sqrt(AvB_B_AKen$varS)
    )
    out$VAR <- out$SD^2
    out$SE.Tau.b <- out$SD/out$D
    
    out$Z <- out$S/out$SD
    out$p <- pnorm(abs(out$Z), lower.tail = FALSE)*2
    
    ret$table[[i]] <- out		
    ret$matrix[[i]] <- tau_m
    ret$tau_u[[i]] <- c("A vs. B + Trend B - Trend A" = out["A vs. B + Trend B - Trend A","Tau"])
    
  }
  weight.t <- c()
  weight.v <- c()
  for(i in 1:N) {
    weight.v <- c(weight.v,1/ret$table[[i]]["A vs. B + Trend B - Trend A","VAR"])
    weight.t <- c(weight.t,ret$table[[i]]["A vs. B + Trend B - Trend A","Tau"])
  }
  ret$Overall_tau_u <- c("A vs. B + Trend B - Trend A" = sum(weight.v*weight.t)/sum(weight.v))
  
  weight.t <- c()
  weight.v <- c()
  for(i in 1:N) {
    weight.v <- c(weight.v,1/ret$table[[i]]["A vs. B - Trend A","VAR"])
    weight.t <- c(weight.t,ret$table[[i]]["A vs. B - Trend A","Tau"])
  }
  ret$Overall_tau_u <- c(ret$Overall_tau_u, "A vs. B - Trend A" = sum(weight.v*weight.t)/sum(weight.v))
  names(ret$table) <- names(data)
  names(ret$tau_u) <- names(data)
  
  class(ret) <- c("sc","TAU-U")
  
  ret
}
