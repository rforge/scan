

estimateSC <- function(data, s = NULL, rtt = NULL) {

  data <- .SCprepareData(data)
  cases <- length(data)
  case.names <- names(data)
  if (is.null(case.names))
    case.names <- paste("Case",1:cases, sep = "")

  B.start <- unlist(lapply(data, function(x) sum(x$phase == "A") + 1))
  MT <- unlist(lapply(data, function(x) length(x$mt)))
  
  d.level <- c()
  d.slope <- c()
  d.trend <- c()
  m <- c()
  rtt <- c()
  error <- c()
  
  for(i in 1:cases) {
    plm.model <- plm(data[i])$full
    res <- coef(plm.model)
    m <- c(m,res[1])
    d.trend <- c(d.trend,res[2])
    d.level <- c(d.level,res[3])
    d.slope <- c(d.slope,res[4])
    error <- c(error, var(plm.model$residual))
  }

  if(cases > 2 && is.null(s))
    s <- sd(m, na.rm = TRUE)
  
  if(is.null(rtt))
    rtt <- 1-(error/s^2)
  d.level <- d.level / s
  d.slope <- d.slope / s
  d.trend <- d.trend / s
  out <- list(N = cases, case.names = case.names, MT = MT, B.start = B.start, m = m, s = s, d.level = d.level, d.slope = d.slope, d.trend = d.trend, rtt = rtt)
  class(out) <- c("sc","parameters")
  out
}