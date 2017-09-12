
overlapSC <- function(data, decreasing = FALSE, phases = c("A","B")) {
  data.list <- .SCprepareData(data)

  keep <- keepphasesSC(data.list, phases = phases)

  data.list <- keep$data
  
  design <- rle(as.character(data.list[[1]]$phase))$values
  N <- length(data.list)

  case.names <- names(data.list)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  
  VAR <- c("PND","PEM","PET","NAP","NAP.rescaled","PAND","TAU_U","Diff_mean", "Diff_trend","SMD")
  d.f <- as.data.frame(matrix(nrow = N, ncol = length(VAR)))
  colnames(d.f) <- VAR
  rownames(d.f) <- c(case.names)

  for(i in 1:N) {
    data <- data.list[[i]]
    d.f$PND[i] <- pnd(data, decreasing = decreasing)$PND
    d.f$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    d.f$PET[i] <- pet(data, decreasing = decreasing)$PET
    d.f$NAP[i] <- nap(data, decreasing = decreasing)$NAP
    d.f$NAP.rescaled[i] <- nap(data, decreasing = decreasing)$NAP.rescaled
    d.f$PAND[i] <- pand(data, decreasing = decreasing)$PAND
    d.f$TAU_U[i] <- tauUSC(data)$Overall_tau_u[2]
    
    A <- data$values[data$phase == "A"]
    B <- data$values[data$phase == "B"]
    d.f$Diff_mean[i] <- mean(B, na = TRUE) - mean(A, na = TRUE)
    d.f$SMD[i] <- (mean(B, na = TRUE) - mean(A, na = TRUE)) / sd(A, na = TRUE)
    
    A.MT <- data$mt[data$phase == "A"]
    B.MT <- data$mt[data$phase == "B"]
    d.f$Diff_trend[i] <- coef(lm(B~I(B.MT-B.MT[1]+1)))[2] - coef(lm(A~I(A.MT-A.MT[1]+1)))[2]
    
  }
  

  out <- list(overlap = d.f, phases.A = keep$phases.A, phases.B = keep$phases.B, design = design)
  class(out) <- c("sc","overlap")
  out
}
