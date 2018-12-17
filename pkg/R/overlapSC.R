#' Overlap indices for single-case data
#' 
#' The \code{overlapSC} function provides the most common overlap indices for
#' single-case data and some additional statistics.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second to the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @return \item{overlap}{A data frame consisting of the following indices for
#' each single-case for all cases: PND, PEM, PET, NAP, PAND, Tau-U (A vs. B -
#' Trend A), Diff_mean, Diff_trend, SMD.}
#' @author Juergen Wilbert
#' @examples
#' 
#' ## Display overlap indices for one single-case
#' overlapSC(Huitema2000, decreasing = TRUE)
#' 
#' ## Display overlap indices for six single-cases
#' overlapSC(GruenkeWilbert2014)
#' 
#' ## Combining phases for analyszing designs with more than two phases   
#' overlapSC(exampleA1B1A2B2, phases = list(c("A1","A2"), c("B1","B2")))
#' 
#' ## Write overlap indices to .csv-file
#' overl <- overlapSC(Waddell2011)
#' write.csv(overl$overlap, file = "overlap_indices.csv")
#' 
overlapSC <- function(data, decreasing = FALSE, phases = c("A","B")) {
  data.list <- .SCprepareData(data)
  ATTRIBUTES <- attributes(data.list)
  keep <- .keepphasesSC(data.list, phases = phases)

  data.list <- keep$data
  
  design <- rle(as.character(data.list[[1]]$phase))$values
  N <- length(data.list)

  case.names <- names(data.list)

  VAR <- c("PND","PEM","PET","NAP","NAP.rescaled","PAND","TAU_U","Diff_mean", "Diff_trend","SMD")
  d.f <- as.data.frame(matrix(nrow = N, ncol = length(VAR)))
  colnames(d.f) <- VAR
  rownames(d.f) <- c(case.names)

  for(i in 1:N) {
    data <- data.list[[i]]
    d.f$PND[i] <- pnd(data, decreasing = decreasing)$PND
    d.f$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    d.f$PET[i] <- pet(data, decreasing = decreasing)$PET
    d.f$NAP[i] <- nap(data, decreasing = decreasing)$nap$NAP[1]
    d.f$NAP.rescaled[i] <- nap(data, decreasing = decreasing)$nap$Rescaled[1]
    d.f$PAND[i] <- pand(data, decreasing = decreasing)$PAND
    #d.f$TAU_U[i] <- tauUSC(data)$Overall_tau_u[2]
    d.f$TAU_U[i] <- tauUSC(data)$table[[1]]["A vs. B + Trend B - Trend A","Tau"]
    
    A <- data$values[data$phase == "A"]
    B <- data$values[data$phase == "B"]
    d.f$Diff_mean[i] <- mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)
    d.f$SMD[i] <- (mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)) / sd(A, na.rm = TRUE)
    
    A.MT <- data$mt[data$phase == "A"]
    B.MT <- data$mt[data$phase == "B"]
    d.f$Diff_trend[i] <- coef(lm(B~I(B.MT-B.MT[1]+1)))[2] - coef(lm(A~I(A.MT-A.MT[1]+1)))[2]
    
  }
  

  out <- list(overlap = d.f, phases.A = keep$phases.A, phases.B = keep$phases.B, design = keep$design[[1]]$values)
  class(out) <- c("sc","overlap")
  attr(out, "var.phase") <- ATTRIBUTES$var.phase
  attr(out, "var.mt") <- ATTRIBUTES$var.mt
  attr(out, "var.values") <- ATTRIBUTES$var.values
  
  out
}
