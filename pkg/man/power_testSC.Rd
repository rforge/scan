% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power_testSC.R
\name{power_testSC}
\alias{power_testSC}
\title{Empirical power analysis for single-case data}
\usage{
power_testSC(
  design,
  stat = c("plm_level", "rand", "tauU"),
  n_sim = 100,
  alpha = 0.05
)
}
\arguments{
\item{design}{An object created by design_rSC}

\item{stat}{Defines the tests the power analysis is based on. The
default \code{stat = c("plm_level", "rand", "tauU")} computes a power analysis based on
\code{\link{tauUSC}}, \code{\link{randSC}} and \code{\link{plm}} analyses. Further
possibilities are: "plm_slope", "plm_poisson_level", "plm_poisson_slope", "hplm_level", 
"hplm_slope", "base_tau".}

\item{n_sim}{Number of sample studies created for the the Monte-Carlo study.
Default is \code{n = 100}}

\item{alpha}{Alpha level used to calculate the proportion of significant
tests. Default is \code{alpha = 0.05}.}
}
\description{
The \code{power_testSC} command conducts a Monte-Carlo study on the
test-power and alpha-error of a set of single-cases. The distribution 
values of the Monte-Carlo sample are either specified by the user or 
estimated based on actual data.
}
\examples{

## Assume you want to conduct a single-case study with 15 MTs, using a highly reliable test,
## an expected level effect of \eqn{d = 1.4}, and randomized start points between MTs 5
## and 12 can you expect to identify the effect using plm or randomization test?
design <- design_rSC(
  n = 1, phase.design = list(A = 6, B = 9), 
  rtt = 0.8, level = 1.4
)
res <- power_testSC(design, n_sim = 10)

## Would you achieve higher power by setting up a MBD with three cases?
design <- design_rSC(
  n = 3, phase.design = list(A = 6, B = 9), 
  rtt = 0.8, level = 1.4
)
res <- power_testSC(design, n_sim = 10, stat = c("hplm_level", "rand"))

}
\seealso{
\code{\link{plm}}, \code{\link{randSC}}
}
\author{
Juergen Wilbert
}
