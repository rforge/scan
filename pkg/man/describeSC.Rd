\name{describeSC}
\alias{describeSC}
\title{
Descriptive statistics for single-case data
}
\description{
The describeSC function provides common descriptive statistics for single-case data.
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
  \item{A list of descriptive statistics.}
}
\author{
Juergen Wilbert
}
\seealso{
\code{\link{writeSC}}.
}
\examples{
## descriptive statistics for one single-case
describeSC(Waddell2011)

## descriptive statistics for a list of three single-cases
describeSC(Grosche2011)

## write descriptive statistics to .csv-file
descr <- describeSC(Waddell2011)
writeSC(descr[1], filename = "descriptives.csv")

set.seed(1234)
dat <- rSC(5, B.start = round(runif(5,5,10)), d.level = 1.0, d.slope = 0.1, d.trend = 0.05, 
rtt = 0.75, round = 0, random.names = TRUE)
describeSC(dat)

set.seed(1233)
dat <- rSC(5, B.start = round(runif(5,5,10)), d.level = -0.4, d.slope = 0.0, d.trend = 0.0, 
rtt = 0.70, random.names = TRUE)
describeSC(dat, decreasing = TRUE)

}

