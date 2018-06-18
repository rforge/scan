#' Percent exceeding the trend
#' 
#' The \code{pet} function provides the percentage of phase B data points
#' exceeding the prediction based on the phase A trend. A binomial test against
#' a 50/50 distribution is computed. Furthermore, the percentage of phase B
#' data points exceeding the upper (or lower) 95 percent confidence interval of
#' the predicted progress is computed.
#' 
#' 
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @param ci Width of the confidence interval. Default is \code{ci = .95}.
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second and the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @return \item{PET}{Percent exceeding the trend.} \item{PET.ci}{Percent
#' exceeding the upper / lower 95\%-CI boundary.} \item{p}{P value of Binomial
#' Test.} \item{ci.percent}{Width of confidence interval in percent.}
#' \item{se.factors}{Standard error.} \item{N}{Number of cases.}
#' \item{decreasing}{Logical argument from function call (see \code{Arguments}
#' above).} \item{case.names}{Assigned name of single-case.} \item{phases}{-}
#' @author Juergen Wilbert
#' @seealso \code{\link{overlapSC}}, \code{\link{describeSC}},
#' \code{\link{nap}}, \code{\link{pand}}, \code{\link{pem}}, \code{\link{pnd}}
#' @examples
#' 
#' ## Calculate the PET and use a 99%-CI for the additional calculation
#' dat <- rSC(n = 5, slope = 0.2)
#' pet(dat, ci = .99)
#' 
pet <- function(data, ci = 0.95, decreasing = FALSE, phases = c("A","B")) {
  
  data <- .SCprepareData(data)
  data <- .keepphasesSC(data, phases = phases)$data
  
  N <- length(data)
  
  if (ci != 0) se.factor <- qnorm(ci) else se.factor <- 0 
  
  pet <- rep(NA, N)
  pet.ci <- rep(NA, N)
  p <- rep(NA, N)
  for(i in 1:N) {
    model <- lm(values~mt, data = data[[i]][data[[i]][,"phase"] == "A",], na.action = na.omit)
    B <- data[[i]][data[[i]][,"phase"] == "B",]
    res <- predict(model, B, se.fit = TRUE)
    nB <- nrow(B)
    if(!decreasing) {
      pet.ci[i] <- mean(B$values > (res$fit + res$se.fit * se.factor), na.rm = TRUE)*100
      pet[i] <- mean(B$values > res$fit, na.rm = TRUE)*100
      p[i] <- binom.test(sum(B$values > res$fit, na.rm=TRUE), nB, alternative = "greater")$p.value
    } else {
      pet.ci[i] <- mean(B$values < (res$fit - res$se.fit * se.factor), na.rm = TRUE)*100
      pet[i] <- mean(B$values < res$fit, na.rm = TRUE)*100
      p[i] <- binom.test(sum(B$values < res$fit, na.rm = TRUE), nB, alternative = "less")$p.value
    }
  }
  if(is.null(names(data)))
    names(data) <- paste("Case",1:N)
  
  out <- list(PET = pet, PET.ci = pet.ci, p = p, ci.percent = ci * 100, se.factors = se.factor, N = N, decreasing = decreasing, case.names = names(data))
  class(out) <- c("sc","PET")
  out
}
