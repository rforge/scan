\name{power.testSC}
\alias{power.testSC}
\title{Empirical Power Analysis for Single Cases}
\description{
The power.testSC command conducts a Monte-Carlo study on the test-power and alpha-error of a randomization-test and a piecewise-regression model. The distribution values of the Monte-Carlo sample is either defined or automatically estimated based on the data of an actual study.
}
\usage{
power.testSC(data = NULL, stat = c("rand.test", "plm"), 
             test.parameter = c("level", "slope"), rand.test.stat = c("Mean B-A", "B"),
             cases = NULL, rtt = 0.8, d.level = NULL, 
             d.slope = NULL, MT = NULL, B.start = NULL, d.trend = NULL, n = 100, 
             limit = 5, m = 50, s = 10, startpoints = NA, extreme.p = 0, 
             extreme.d = c(-4, -3), exclude.equal = "auto", alpha = 0.05, 
             distribution = "normal", concise = TRUE, silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames. If data are provided, the power-analysis will be based on the design of these data and on the estimation of the effect sizes within the data.
}
  \item{stat}{
Defines the statistics the power analysis is computed for. The default stat = c("rand.test","plm") computes a power-analysis for the randomization and the plm analysis.Further possibilities are "hplm" for a hierarchiacal linear regression model and "plm.poisson" for a generalized piecewise-regression model with the assumption of Poisson distributed error terms.
}
  \item{test.parameter}{
Indicated whether the power and alpha error for a level effect, a slope effect, or both effects should be estimated.
}
  \item{rand.test.stat}{
Defines the statistic a randomization test is based on. The first values stipulates the statistic for the level-effect computation and the second value for the slope-effect computation. Deafult is rand.test.stat = c("Mean B-A","B").
}
  \item{cases}{
Number of cases per study.
}
  \item{rtt}{
Reliability of the underlying simulated measurements.
}
  \item{d.level}{
A single level increase with the beginning of the B-Phase in standard deviations.
}
  \item{d.slope}{
Amount of increase in standard deviations per measurement time starting with the Beginning of the B-phase (e.g., d.slope = 0.1 generates an incremental increase of 0.1 standard deviations for each measurement of the B-phase).
}
  \item{MT}{
Measurement times of single cases' random study.
}
  \item{B.start}{
Starting values of the B-phase. A single value (e.g., B.start = 6) defines the B.start for all studies and cases. A vector of starting values is given with the chain command (e.g., B.start = c(6, 7, 8)). A value between 0 and 1 is interpreted as a proportion (e.g., B.start = c(0.3, 0.5, 0.8) are starting point at 30, 50, and 80 percent of the length defined in MT).
}
  \item{d.trend}{
Defines a trend in d per measurement across all measurements. E.g., trend = 0.1 is an increase of 0.1 standard deviations per measurement.
}
  \item{n}{
Number of sample studies created in the the Monte-Carlo study.
}
  \item{limit}{
Minimal length of the A- und B-phase in the randomization sample.
}
  \item{startpoints}{
Alternative to the limit parameter startpoints exactly defines the possible start points of the B-phase. E.g., startpoints = 4:9 restricts possible start points of the B-phase in the randomization design to measurements 4 to 9. startpoints arguments overwrite the limit parameter.
}
  \item{m}{
Mean of the sample distribution the data are drawn from. 
}
  \item{s}{
Standard deviation of the sample distribution the data are drawn from. 
}


  \item{extreme.p}{
Probability of extreme values (e.g., extreme.p = 0.05 gives a 5 percent probability of an extreme value.
}
  \item{extreme.d}{
Range of an extreme value in d (e.g., extreme.d = c(-7,-6) results in extreme values within a range of -7 and -6 standard deviations).  Default is c(-4,-3). Caution: the first value must be smaller than the second, otherwise the procedure will fail.
}
  \item{exclude.equal}{
If set FALSE, random distribution value that are equal to the observed distribution are counted as values of the distribution of the null-hypothesis. That is, they decrease the probability of rejecting the null-hypothesis (increase the p-value). Default is exclude.equal = "auto" (FALSE for multiple-baseline designs and TRUE for single baseline designs).
}
  \item{alpha}{
Alpha level used to calculate the proportion of significant tests. Default is 0.05.
}
  \item{distribution}{
Indicated whether the random sample is based on a "normal" (e.g., gaussian) distribution or on a "poisson" distribution.
}
  \item{concise}{
If set TRUE, no effects on the generated random samples are given during the computation.
}
  \item{silent}{
If set TRUE, the results are not printed after computation.
}
}
\author{Juergen Wilbert}
\examples{
#Assume you want to conduct a study with 15 measurements.
#The test is highly reliable and you expect an intervention effect of d.level = 1.4.
#The starting point of the intevention phase is randomly set between the 
#5th and the 12th measurement. Can you expect to identify the effect with a 
#plm or a randomization test (for a more precise estimation, set n = 1000)?


power.testSC(MT = 15, B.start = round(runif(100,5,12)), test.parameter = "level", 
             d.level = 1.4, rtt = 0.8, n = 10)

# Would the testpower be higher if you would set up a multiple baseline design (for
# a more precise estimation, set n = 1000)?

power.testSC(cases = 3, MT = 15, stat = c("rand.test","hplm"), 
             B.start = round(runif(300,5,12)), test.parameter = "level", 
             d.level = 1.4, rtt = 0.8, n = 10, startpoints = 5:12)

}
