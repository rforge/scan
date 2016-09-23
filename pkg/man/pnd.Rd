\name{pnd}
\alias{pnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Percentage of non-overlapping data}
\description{
This function returns the percentage of non-overlapping data. Due to its error-proneness the PND should not be used, but NAP or PAND instead (see Parker & Vannest, 2009).
}
\usage{
pnd(data, decreasing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{decreasing}{If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.}
}
\value{
  \item{PND}{Percentage of non-overlapping data.}
}
\author{
Juergen Wilbert
}
\examples{
## Calculate PEM for multiple single-case data
pnd(Huber2014)
}

