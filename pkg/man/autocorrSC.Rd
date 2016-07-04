\name{autocorrSC}
\alias{autocorrSC}
\title{
Autocorrelation for single-case data
}
\description{
The autocorrSC function provides autocorrlations for both phases of a ingle case and the conjoined data of both phases up to a given lag.
}
\usage{
autocorrSC(data, lag.max = 3)
}

\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See makeSCDF to learn about this format.
}
  \item{lag.max}{
The lag up to which an autocorrelation will be computed.
}
}
\value{
  \item{autocorr}{A data frame depicting autocorrelations for each single case for each phase. WHen the given maximim lag exceeds the length of a phase minus one, an NA is returned for this cell.}
  
}
\author{
Juergen Wilbert
}
\examples{
set.seed(1235)
dat <- rSC(3, d.slope = 0.3, round = 0, random.names = TRUE)
autocorrSC(dat)

}

