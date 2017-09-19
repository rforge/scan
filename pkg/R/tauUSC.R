
tauUSC <- function (data, ties.method = "omit", method = "complete", phases = c("A","B")) {
  data <- .SCprepareData(data)
  data <- keepphasesSC(data, phases = phases)$data
  
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
    
    pos.s <- c("+")
    neg.s <- c("-")
    tie.s <- c("T")
    if(ties.method == "positive") 
      pos.s <- c("+","T")
    if(ties.method == "negative") 
      neg.s <- c("-","T")
    
    
    tau_m[lower.tri(tau_m)] <- ""
    
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
    
    
    if(method == "parker") {
      out$pairs <- c(
        nA*nB, 
        (nA*(nA-1))/2, 
        (nB*(nB-1))/2, 
        (nA*(nA-1))/2 + (nB*(nB-1))/2,
        #(nAB*(nAB-1))/2, 
        nA*nB,
        nA*nB + (nB*(nB-1))/2, 
        nA*nB + (nB*(nB-1))/2
      )
    }
    
    if(method == "complete") {
      out$pairs <- c(
        nA*nB, 
        (nA*(nA-1))/2, 
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
    
    #AvBKen <- Kendall(AB, c(rep(0,nA),rep(1,nB)))
    #AvAKen <- Kendall(A, 1:nA)
    #BvBKen <- Kendall(B, 1:nB)
    #BvB_AKen <- Kendall(c(A,B), c(nA:1,1:nB))
    
    #AvB_B_AKen <- Kendall(c(A, B),c(nA:1,(nA + 1):nAB))
    #AvB_AKen <- Kendall(c(A, B),c(nA:1,rep(nA+1,nB)))
    
    #AvB_BKen <- Kendall(c(A, B),c(rep(0, nA),(nA + 1):nAB))
    #out$D <- c(
    #  out$pairs[1]-out$ties[1]/2,
    #  AvAKen$D,
    #  BvBKen$D,
    #  BvB_AKen$D,
    #  #NA,
    #  AvB_AKen$D,
    #  AvB_BKen$D,
    #  AvB_B_AKen$D
    #)
    
    
    out$Tau <- out$S / out$pairs
    #out$Tau.b <- out$S / out$D
    #out$SD <- c(
    #  sqrt((nA*nB)*(nA+nB+1)/12)*2,
    #  sqrt(Kendall(sample(nA,nA),1:nA)$varS),
    #  sqrt(Kendall(sample(nB,nB),1:nB)$varS),
    #  sqrt(Kendall(sample(nA+nB, nA+nB), c(nA:1,1:nB))$varS),
    #  #NA,
    #  sqrt(AvB_AKen$varS),
    #  sqrt(AvB_BKen$varS),
    #  sqrt(AvB_B_AKen$varS)
    #)
    #out$VAR <- out$SD^2
    #out$Z <- out$S/out$SD
    #out$p <- pnorm(abs(out$Z), lower.tail = FALSE)*2
    
    ret$table[[i]] <- out		
    ret$matrix[[i]] <- tau_m
    ret$tau_u[[i]] <- c("A vs. B + Trend B - Trend A" = out["A vs. B + Trend B - Trend A","Tau"])
  
  }
  #weight.t <- c()
  #weight.v <- c()
  #for(i in 1:N) {
  #  weight.v <- c(weight.v,1/ret$table[[i]]["A vs. B + Trend B - Trend A","VAR"])
  #  weight.t <- c(weight.t,ret$table[[i]]["A vs. B + Trend B - Trend A","Tau"])
  #}
  #ret$Overall_tau_u <- c("A vs. B + Trend B - Trend A" = sum(weight.v*weight.t)/sum(weight.v))
  
  #weight.t <- c()
  #weight.v <- c()
  #for(i in 1:N) {
  #  weight.v <- c(weight.v,1/ret$table[[i]]["A vs. B - Trend A","VAR"])
  #  weight.t <- c(weight.t,ret$table[[i]]["A vs. B - Trend A","Tau"])
  #}
  #ret$Overall_tau_u <- c(ret$Overall_tau_u, "A vs. B - Trend A" = sum(weight.v*weight.t)/sum(weight.v))
  names(ret$table) <- names(data)
  names(ret$tau_u) <- names(data)
  
  class(ret) <- c("sc","TAU-U")
  
  ret
}



