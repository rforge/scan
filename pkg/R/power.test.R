#' Empirical power analysis for single-case data
#' 
#' The \code{power.testSC} command conducts a Monte-Carlo study on the
#' test-power and alpha-error of a randomization-test and a
#' piecewise-regression model.  The distribution values of the Monte-Carlo
#' sample are either specified by the user or estimated based on actual data.
#' 
#' 
#' @param data A single-case data frame or a list of single-case data frames.
#' See \code{\link{makeSCDF}} to learn about this format. If you provide data,
#' the power-analysis will be based on the design of these data and its
#' estimated effect sizes.
#' @param stat Defines the tests the power analysis is computed for. The
#' default \code{stat = c("rand.test","plm")} computes a power analysis for the
#' \code{\link{randSC}} and the \code{\link{plm}} analyses. Further
#' possibilities are \code{"hplm"} for a hierarchiacal linear regression model
#' and \code{"plm.poisson"} for a generalized piecewise-regression model under
#' the assumption of poisson distributed errors.
#' @param test.parameter Indicates whether the power and alpha error for a
#' level effect, a slope effect, or both effects should be estimated. The
#' default setting \code{test.parameter = c("level", "slope")} requests both.
#' @param rand.test.stat Defines the statistic the randomization test is based
#' on. The first values stipulates the statistic for the level-effect
#' computation and the second value for the slope-effect computation. Default
#' is \code{rand.test.stat = c("Mean B-A","B")}. Please see
#' \code{\link{randSC}} for more information on the test statistics.
#' @param cases Number of cases per study.
#' @param rtt Reliability of the underlying simulated measurements. Default is
#' \code{rtt = 0.8}.
#' @param level Defines the level increase (effect size \emph{d}) at the
#' beginning of phase B.
#' @param slope Defines the increase in scores - starting with phase B -
#' expressed as effect size \emph{d} per MT. \code{slope = .1} generates an
#' incremental increase of 0.1 standard deviations per MT for all phase B
#' measurements.
#' @param MT Number of measurements (in each study).
#' @param B.start Phase B starting point. A single value (e.g., \code{B.start =
#' 6}) defines \code{B.start} for all studies and cases. A vector of starting
#' values is given with the chain command (e.g., \code{B.start = c(6, 7, 8)}).
#' A value between 0 and 1 is interpreted as a proportion (e.g., \code{B.start
#' = c(0.3, 0.5, 0.8)} would start phase B at 30, 50, and 80\% of the MTs).
#' @param trend Defines the effect size \emph{d} of a trend per MT added
#' across the whole data-set.
#' @param n Number of sample studies created for the the Monte-Carlo study.
#' Default is \code{n = 100}
#' @param limit Minimal number of data points per phase in the sample. Default
#' is \code{limit = 5}.
#' @param m Mean of the sample distribution the data are drawn from.
#' @param s Standard deviation of the sample distribution the data are drawn
#' from.
#' @param startpoints Alternative to the \code{limit} parameter start points
#' exactly defines the possible start points of phase B (e.g.,
#' \code{startpoints = 4:9} restricts the phase B start points to measurements
#' 4 to 9. \code{startpoints} overruns the \code{limit} parameter.
#' @param extreme.p Probability of extreme values. \code{extreme.p = .05} gives
#' a five percent probability of an extreme value. Default is \code{extreme.p =
#' 0}.
#' @param extreme.d Range for extreme values, expressed as effect size
#' \emph{d}. \code{extreme.d = c(-7,-6)} uses extreme values within a range of
#' -7 and -6 standard deviations. Caution: the first value must be smaller than
#' the second, otherwise the procedure will fail. Default is \code{extreme.d =
#' c(-4,-3)}.
#' @param exclude.equal If set to \code{exclude.equal = FALSE}, random
#' distribution values equal to the observed distribution are counted as
#' null-hypothesis conform. That is, they decrease the probability of rejecting
#' the null-hypothesis (increase the p-value). Default is \code{exclude.equal =
#' "auto"}, which means \code{FALSE} for multiple baseline designs and
#' \code{TRUE} for one single-case.
#' @param alpha Alpha level used to calculate the proportion of significant
#' tests. Default is \code{alpha = 0.05}.
#' @param distribution Indicates whether the random sample is based on a
#' \code{"normal"} or a \code{"poisson"} distribution. Default is
#' \code{distribution = "normal"}.
#' @param silent If set \code{TRUE}, the results are not printed after
#' computation. Default is \code{silent = FALSE}.
#' @param parameters -
#' @author Juergen Wilbert
#' @seealso \code{\link{plm}}, \code{\link{randSC}}
#' @examples
#' 
#' ## Assume you want to conduct a single-case study with 15 MTs, using a highly reliable test,
#' ## an expected level effect of \eqn{d = 1.4}, and randomized start points between MTs 5
#' ## and 12 can you expect to identify the effect using plm or randomization test?
#' 
#' power.testSC(MT = 15, B.start = round(runif(100,5,12)), test.parameter = "level", 
#'              level = 1.4, rtt = 0.8, n = 10)
#' 
#' ## Would you achieve higher power by setting up a MBD with three cases?
#' 
#' power.testSC(cases = 3, MT = 15, stat = c("rand.test","hplm"), 
#'              B.start = round(runif(300,5,12)), test.parameter = "level", 
#'              level = 1.4, rtt = 0.8, n = 10, startpoints = 5:12)
#' 
power.testSC <- function(data = NULL, parameters = NULL,stat = c("rand.test","plm"), 
                         test.parameter = c("level", "slope"), rand.test.stat = c("Mean B-A","B"), 
                         cases = NULL, rtt = NULL, level = NULL, slope = NULL, MT = NULL, 
                         B.start = NULL, trend = NULL, n = 100, limit = 5,  
                         m = NULL, s = NULL, startpoints = NA, extreme.p = 0, extreme.d = c(-4,-3), 
                         exclude.equal = "auto", alpha = 0.05, 
                         distribution = "normal", silent = FALSE) {
  
  return.distribution <- FALSE # depricated parameter 	
  
  if(!is.null(data)) {
    data <- .SCprepareData(data)
    est <- estimateSC(data, s = s, rtt = rtt)
    cases = length(data)
    B.start <- est$B.start
    MT <- est$MT
    level <- est$level
    slope <- est$slope
    trend <- est$trend
    m <- est$m
    s <- est$s
    rtt <- est$rtt
    #if(cases == 2 && is.null(s))
    #  stop("Standard deviation could not be estimated with less than two cases. Please provide a value.\n")
  }
  if(!is.null(parameters)) {
    cases <- parameters$N
    B.start <- parameters$B.start
    MT <- parameters$MT
    level <- parameters$level
    slope <- parameters$slope
    trend <- parameters$trend
    m <- parameters$m
    s <- parameters$s
    rtt <- parameters$rtt
    
  }
  if(is.null(data) && is.null(parameters)) {
    if(is.null(rtt))
      rtt <- 0.8
    if(is.null(level)) level <- 0
    if(is.null(slope)) slope <- 0
    if(is.null(trend)) trend <- 0
    if(is.null(cases)) cases <- 1
    if(is.null(m)) m <- 50
    if(is.null(s)) s <- 10
  }
  
  if(cases == 1 && is.null(s))
    stop("Standard deviation could not be estimated with less than two cases. Please provide a value.\n")
  if (any(stat %in% c("plm","plm.poissonm")) && cases > 1)
    stop("plm models can not be calculated with more than one case. Consider using hplm\n")
  
  if(exclude.equal == "auto") 
    exclude.equal <- ifelse(cases == 1, TRUE, FALSE)
  
  if(!silent)	{
    cat("Compute Monte-Carlo power-analyses with the following parameters:\n\n")
    cat("Stats:\t\t",stat,"\n")
    cat("Sample studies\t",n,"\n")
    cat("Cases per sample",cases,"\n")
    cat("M\t\t",m,"\n")
    cat("SD\t\t",s,"\n")
    cat("MT\t\t",MT,"\n")
    cat("B.start\t\t",sort(unique(B.start)),"\n")
    cat("rtt\t\t",rtt,"\n")
    cat("d level\t\t",level,"\n")
    cat("d slope\t\t",slope,"\n")
    cat("d trend\t\t",trend,"\n")	
    cat("Extreme.p\t",extreme.p,"\n")	
    cat("Extreme.d\t",extreme.d,"\n")	
    cat("Alpha level\t",alpha,"\n")	
    cat("Exclude equal\t",exclude.equal,"\n")
    if (is.na(startpoints[1])) {
      cat("Limit\t\t",limit,"\n")
    } else {
      cat("Startpoints\t\t",startpoints,"\n")
    }
  }
  out <- list()
  
  out$power.tauU.level <- NA
  
  out$power.ranlevel <- NA
  out$power.plm.level <- NA
  out$power.plm.slope <- NA
  out$power.plm.poisson.level <- NA
  out$power.plm.poisson.slope <- NA
  
  out$power.hplm.level <- NA
  out$power.hplm.slope <- NA


  out$alphaerror.tauU.level <- NA
  
  out$alphaerror.ranlevel <- NA
  out$alphaerror.plm.level <- NA
  out$alphaerror.plm.slope <- NA
  out$alphaerror.plm.poisson.level <- NA
  out$alphaerror.plm.poisson.slope <- NA
  
  out$alphaerror.hplm.level <- NA
  out$alphaerror.hplm.slope <- NA
  
  out$rand.test.stat <- rand.test.stat
  out$rand.sample <- NA
  
  res <- .power.testSC(n = n, MT = MT, B.start = B.start, 
                       rand.test.stat = rand.test.stat, 
                       level = level, slope = slope, 
                       extreme.p = extreme.p, extreme.d = extreme.d,
                       m = m, s = s, cases = cases, trend = trend, 
                       rtt = rtt, alpha = alpha, limit = limit,  
                       startpoints = startpoints, exclude.equal = exclude.equal, 
                       return.distribution = return.distribution, stat = stat, 
                       test.parameter = test.parameter, distribution = distribution)
  
  if(return.distribution) 
    out$rand.sample <- res$rand.sample
  
  if(all(level == 0)) {
    if(any(stat == "rand.test"))
      out$alphaerror.ranlevel <- res$ranlevel
    if(any(stat == "plm"))
      out$alphaerror.plm.level <- res$plm.level
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.level <- res$plm.poisson.level
    if(any(stat == "hplm"))
      out$alphaerror.hplm.level <- res$hplm.level
    if(any(stat == "tauU"))
      out$alphaerror.tauU.level <- res$tauU.level
    
  } else {
    if(any(stat == "rand.test"))
      out$power.ranlevel <- res$ranlevel
    if(any(stat == "plm"))
      out$power.plm.level <- res$plm.level
    if(any(stat == "plm.poisson"))
      out$power.plm.poisson.level <- res$plm.poisson.level
    if(any(stat == "hplm"))
      out$power.hplm.level <- res$hplm.level
    if(any(stat == "tauU"))
      out$power.tauU.level <- res$tauU.level
    
  }
  
  
  if(all(slope == 0)) {
    if(any(stat == "plm"))
      out$alphaerror.plm.slope <- res$plm.slope
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.slope <- res$plm.poisson.slope
    if(any(stat == "hplm"))
      out$alphaerror.hplm.slope <- res$hplm.slope
  } else {
    if(any(stat == "plm"))
      out$power.plm.slope <- res$plm.slope
    if(any(stat == "plm.poisson"))
      out$power.plm.poisson.slope <- res$plm.poisson.slope
    if(any(stat == "hplm"))
      out$power.hplm.slope <- res$hplm.slope
  }
  
  
  if(any(level != 0)) {
    res <- .power.testSC(n = n, MT = MT, B.start = B.start, 
                         rand.test.stat = rand.test.stat, level = 0, 
                         slope = slope, extreme.p = extreme.p, 
                         extreme.d = extreme.d,m = m, s = s, 
                         cases = cases, trend = trend, rtt = rtt, 
                         alpha = alpha, limit = limit,  
                         startpoints = startpoints, exclude.equal = exclude.equal, 
                         stat = stat, test.parameter = test.parameter, 
                         distribution = distribution)
    if(any(stat == "rand.test"))
      out$alphaerror.ranlevel <- res$ranlevel
    if(any(stat == "plm"))
      out$alphaerror.plm.level <- res$plm.level
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.level <- res$plm.poisson.level
    if(any(stat == "hplm"))
      out$alphaerror.hplm.level <- res$hplm.level
    if(any(stat == "tauU"))
      out$alphaerror.tauU.level <- res$tauU.level
    
  }
  
  if(any(slope != 0)) {
    res <- .power.testSC(n = n, MT = MT, B.start = B.start, 
                         rand.test.stat = rand.test.stat, 
                         level = level, slope = 0, 
                         extreme.p = extreme.p, extreme.d = extreme.d,
                         m = m, s = s, cases = cases, trend = trend, 
                         rtt = rtt, alpha = alpha, limit = limit,  
                         startpoints = startpoints, exclude.equal = exclude.equal, 
                         stat = stat, test.parameter = test.parameter, 
                         distribution = distribution)
    if(any(stat == "plm"))
      out$alphaerror.plm.slope <- res$plm.slope
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.slope <- res$plm.poisson.slope
    if(any(stat == "hplm"))
      out$alphaerror.hplm.slope <- res$hplm.slope
  }
  
  class(out) <- c("sc","power")
  out
}


### experimetal functions

.power.testSC <- function(n = NA, MT = NA, B.start = NA, 
                          level = NA, slope = NA, cases = NA, 
                          trend = NA, extreme.p = 0, rand.test.stat, 
                          extreme.d = c(-4,-3),rtt = NA, alpha = NA, 
                          m = NA, s = NA, limit = NA, startpoints = NA, 
                          exclude.equal = NA, return.distribution = FALSE, 
                          stat, test.parameter, distribution) {
  
  out <- list()
  out$ranlevel <- NA
  out$plm.slope <- NA
  out$plm.level <- NA
  out$plm.poisson.slope <- NA
  out$plm.poisson.level <- NA
  out$tauU.level <- NA
  
  out$rand.sample <- NA
  
  rand.sample <- list()
  for(i in 1:n) {
    rand.sample[[i]] <- rSC(n = cases, MT = MT, m = m, s = s,
                            B.start = B.start, level = level, 
                            slope = slope, trend = trend, extreme.p = extreme.p, 
                            extreme.d = extreme.d, rtt = rtt, 
                            distribution = distribution)
  }
  #if(cases == 1)
  #  rand.sample <- lapply(rand.sample, list)
  
  if(any(stat == "rand.test")) {
    if(any(test.parameter == "level")) {
      p.ranlevel <- unlist(lapply(rand.sample, function(x) randSC(x, statistic = rand.test.stat[1], number = 100, exclude.equal = exclude.equal, limit = limit, startpoints = startpoints, output = "p")))
      out$ranlevel <- mean(p.ranlevel <= alpha, na.rm = TRUE)
    } else out$ranlevel <- NA
  }
  
  if(any(stat == "tauU")) {
    if(any(test.parameter == "level")) {
      p.tauU.level <- unlist(lapply(rand.sample, function(x) tauUSC(x, method = "parker")$table[[1]][6,12]))
      out$tauU.level <- mean(p.tauU.level <= alpha, na.rm = TRUE)
    } else out$tauU.level <- NA
  }
  
  if(any(stat == "plm")) {
    if(any(test.parameter == "level")) {
      p.plm.level <- unlist(lapply(rand.sample, function(x) .plm.mt(x, type = "level p")))
      out$plm.level <- mean(p.plm.level <= alpha, na.rm = TRUE)
    } else out$plm.level <- NA
    
    
    if(any(test.parameter == "slope")) {
      p.plm.slope <- unlist(lapply(rand.sample, function(x) .plm.mt(x, type = "slope p")))
      out$plm.slope <- mean(p.plm.slope <= alpha, na.rm = TRUE)
    } else out$plm.slope <- NA
  }
  
  if(any(stat == "plm.poisson")) {
    if(any(test.parameter == "level")) {
      p.plm.poisson.level <- unlist(lapply(rand.sample, function(x) .plm.mt(x, count.data = TRUE, type = "level p")))
      out$plm.poisson.level <- mean(p.plm.poisson.level <= alpha, na.rm = TRUE)
    } else out$plm.poisson.level <- NA
    
    
    if(any(test.parameter == "slope")) {
      p.plm.poisson.slope <- unlist(lapply(rand.sample, function(x) .plm.mt(x, count.data = TRUE, type = "slope p")))
      out$plm.poisson.slope <- mean(p.plm.poisson.slope <= alpha, na.rm = TRUE)
    } else out$plm.poisson.slope <- NA
  }
  
  
  if(any(stat == "hplm")) {
    if(any(test.parameter == "level")) {
      p.hplm.level <- unlist(lapply(rand.sample, function(x) summary(hplm(x, random.slopes = FALSE, ICC = FALSE)$hplm)$tTable[3,5]))
      out$hplm.level <- mean(p.hplm.level <= alpha, na.rm = TRUE)
    } else out$hplm.level <- NA
    
    
    if(any(test.parameter == "slope")) {
      p.hplm.slope <- unlist(lapply(rand.sample, function(x) summary(hplm(x, random.slopes = FALSE, ICC = FALSE)$hplm)$tTable[4,5]))
      out$hplm.slope <- mean(p.hplm.slope <= alpha, na.rm = TRUE)
    } else out$hplm.slope <- NA
  }
  
  
  if(return.distribution)
    out$rand.sample <- rand.sample
  
  out
}

