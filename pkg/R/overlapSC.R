overlapSC <- function(data, decreasing = FALSE, phases = c("A","B")) {
  data.list <- .SCprepareData(data)
  data.list <- keepphasesSC(data.list, phases = phases)$data
  N <- length(data.list)
  case.names <- names(data.list)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  
  VAR <- c("PND","PEM","PET","NAP","NAP.rescaled","PAND","TAU_U")
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
    d.f$TAU_U[i] <- tauUSC(data)$tau_u
  }
  
  
  out <- list(overlap = d.f, phases = phases)
  class(out) <- c("sc","overlap")
  out
}
