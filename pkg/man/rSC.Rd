\name{rSC}
\alias{rSC}
\title{Random sample generator}
\description{
Generate random single- and multiple-baseline samples.
}
\usage{
rSC(n = 1, MT = 20, B.start = 6, m = 50, s = 10, prob = 0.5, d.trend = 0, d.level = 0, 
    d.slope = 0, rtt = 0.8, concise = TRUE, cases = 1, round = NA, extreme.p = 0, 
    extreme.d = c(-4, -3), missing.p = 0, distribution = "normal", 
    start.values.fixed = FALSE, random.names = FALSE, output.long = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of studies to be created (default is n = 1).
}
  \item{MT}{
Measurement times of single cases in each study.
}
  \item{B.start}{
Starting values of the B-phase. A single value (e.g., B.start = 6) defines the B.start for all studies and cases. A vector of starting values is given with the chain command (e.g., B.start = c(6, 7, 8)). If the number of n*cases surpasses the length of the vector, values are repeated. 
}
  \item{m}{
Mean of the sample distribution the data are drawn from. A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated.
}
  \item{start.values.fixed}{
If set TRUE, the true value at the first measurement of each case is set to the value defined in the m argument. If set FALSE, the true values of each case is drawn from a distribution (defined in the argument distribution) with the expected value of the argument m. Default is FALSE.
}

  \item{s}{
Standard deviation of the sample distribution the data are drawn from. A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated.
}
  \item{prob}{
When the paramter distribution is set to "binomial", prob indicates the probability of the occurence of an event.
}


  \item{d.trend}{
Defines a trend in d per measurement across all measurements. A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated. E.g., d.trend = 0.1 is an increase of 0.1 standard deviations per measurement. In case of a binomial or poisson distribution, d.trend indicates the increase in points/counts per measurement.  
}
  \item{d.level}{
A single level increase with the beginning of the B-Phase in standard deviations. A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated. In case of a binomial or poisson distribution, d.level indicates the increase in points/counts. 
}
  \item{d.slope}{
Amount of increase in standard deviations per measurement time starting with the Beginning of the B-phase (e.g., d.slope = 0.1 generates an incremental increase of 0.1 standard deviations for each measurement of the B-phase). A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated. In case of a binomial or poisson distribution, d.slope indicates the increase in points/counts. 
}
  \item{rtt}{
Reliability of the underlying simulated measurements (default is rtt = 0.8). A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated. In case of a binomial or poisson distribution rtt does have no effect on the generated data. 
}
  \item{concise}{
If set TRUE, no information about the generated sample is given.
}
  \item{cases}{
Number of cases per study. E.g., n = 10, cases = 3, B.start = c(7,9,11) creates 10 multiple-baseline designs with 3 persons (Starting point of the B-phase at 7, 9, 11, respectively) each.
}

\item{round}{
Rounds the measurement values to a specific decimal position (e.g., round = 2).
}
  \item{extreme.p}{
Probability of extreme values (e.g., extreme.p = 0.05 gives a 5 percent probability of an extreme value. A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated.   
}
  \item{extreme.d}{
Range of an extreme value in d (e.g., extreme.d = c(-7,-6) results in extreme values within a range of -7 and -6 standard deviations).  In case of a binomial or poisson distribution, extreme.d indicates points/counts. Caution: the first value must be smaller than the second, otherwise the procedure will fail.}
  \item{missing.p}{
Proportion of missing values (e.g., missing.p = 0.1 renders 10 percent of all values as missing). A vector of values defines alterating values for each case. If the number of n*cases surpasses the length of the vector, values are repeated.   
}


  \item{distribution}{
Possible values are "normal", "binomial", or "poisson". If set to normal, the sample is created based on the assumption of a normal distribution of values (with the parameters m and s as the mean and the standard deviation), including a measurement error defined in the parameter rtt. If set to "binomial", data are drawn from a binomial distribution with the expectation value defined by the m parameter. This setting is useful for generating criterial data like correct answers in a test. If set to "poisson", data are drawn from a poisson distribution representing count-data as are often collected in behavioural observations. A measurement error is not included and the m parameter is used as the expectation value of the poisson distribution lambda.
}
  \item{random.names}{
Just a gimmick. If set TRUE cases have random first names. The selection of names is drawn from a list of the 2000 most popular names for births in the U.S.A. 2012 (1000 names for boys and 1000 names for girls). 
}
  \item{output.long}{
If set TRUE, true, trend, level, slope, and error values are included for each case.
}
}
\author{Juergen Wibert}
\examples{
set.seed(1234)
dat <- rSC(n = 3, rtt = 0.75, round = 1, d.slope = 0.1, extreme.p = 0.1, 
           missing.p = 0.1, random.names = TRUE)
describeSC(dat)

set.seed(1234)
dat <- rSC(n = 50, rtt = 0.6, round = 1, d.trend = 0.05, 
           d.level = 0.5, d.slope = 0.1)
hplm(dat)


set.seed(1234)
dat <- rSC(n = 3, B.start = c(6,10,14),MT = c(12,20,22),m = 10, distribution = "poisson", d.level = 5)

}
