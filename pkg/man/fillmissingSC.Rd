\name{fillmissingSC}
\alias{fillmissingSC}
\title{Replace missing measurement times in single-case data}
\description{
This function replaces missing measurement times in single-case data.
}
\usage{
fillmissingSC(data, interpolation = "linear", na.rm = TRUE)
}
\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{interpolation}{
Alternative options not yet included. Default is \code{interpolation = "linear"}.
}
  \item{na.rm}{
If set \code{TRUE}, \code{NA} values are also interpolated. Default is \code{na.rm = TRUE}.
}
}
\details{This procedure is recommended if there are gaps between measurement times (e.g. MT: 1, 2, 3, 4, 5, ... 8, 9) or explicitly missing values in your single-case data set and you want to calculate overlap indices (\code{overlapSC}) or a randomization test (\code{randSC}).
}
\value{Returns a data frame (for each single-case) with missing data points interpolated.
}
\author{Juergen Wilbert}
\examples{
## In his study, Grosche (2011) could not realize measurements each single week for 
## all participants. During the course of 100 weeks, about 20 measurements per person 
## at different times were administered.

## fill missing values in a single-case dataset with discontinuous measurement times
Grosche2011filled <- fillmissingSC(Grosche2011)
## graphical comparison of the two datasets
plotSC(c(Original = Grosche2011[2], Filled = Grosche2011filled[2]))


## fill missing values in a single-case dataset that are NA
set.seed(1234)
Maggie <- rSC(d.level = 1.0)
Maggie_n <- Maggie
replace.positions <- c(10,16,18)
Maggie_n[[1]][replace.positions,"values"] <- NA
Maggie_f <- fillmissingSC(Maggie_n)
plotSC(c("original" = Maggie, "missing" = Maggie_n, "interpolated" = Maggie_f),
       marks = list(positions = replace.positions))
}

