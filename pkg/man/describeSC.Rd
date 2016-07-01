\name{describeSC}
\alias{describeSC}
\title{
Descriptive statistics for single-case data
}
\description{
The describeSC function provides important descriptive statistics for each dataset seperated for A- and B-phase.
}
\usage{
describeSC(data, decreasing = FALSE)
}

\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See makeSCDF to learn about this format.
}
  \item{decreasing}{
If you expect the data to be lower in the B-phase, set decreasing = TRUE.
}
}
\value{
  \item{n.A}{Number of values in phase A.}
  \item{n.B}{Number of values in phase B.}
  \item{n.AB}{Number of values.}
  \item{Missing.A}{Number of missing values in phase A.}
  \item{Missing.B}{Number of missing values in phase B.}
  \item{Missing.AB}{Number of missing values.}
  \item{Mean.A}{Mean of phase A values.}
  \item{Mean.B}{Mean of phase B values.}
  \item{Mean.dif}{Difference between the means of phase A and B.}
  \item{Median.A}{Median of phase A values.}
  \item{Median.B}{Median of phase B values.}
  \item{Min.A}{Minimum of phase A values.}
  \item{Min.B}{Minimum of phase B values.}
  \item{Max.A}{Maximum of phase A values.}
  \item{Max.B}{Maximum of phase B values.}
  \item{SD.A}{Standard Deviation of phase A values.}
  \item{SD.B}{Standard Deviation of phase B values.}
  \item{SD.AB}{Standard Deviation of all values.}
  \item{Autocor.A}{Autocorrelation within phase A.}
  \item{Autocor.B}{Autocorrelation within phase B.}
  \item{Trend.A}{}
  \item{Trend.B}{}
  \item{Trend.AB}{}
  \item{Trend.dif}{}
  \item{SMD}{}
}
\author{
Juergen Wilbert
}
\examples{
set.seed(1234)
dat <- rSC(5, B.start = round(runif(5,5,10)), d.level = 1.0, d.slope = 0.1, d.trend = 0.05, 
rtt = 0.75, round = 0, random.names = TRUE)
describeSC(dat)

set.seed(1233)
dat <- rSC(5, B.start = round(runif(5,5,10)), d.level = -0.4, d.slope = 0.0, d.trend = 0.0, 
rtt = 0.70, random.names = TRUE)
describeSC(dat, decreasing = TRUE)

}

