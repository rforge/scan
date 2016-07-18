\name{tauUSC}
\alias{tauUSC}
\title{Tau-U for single-case data}
\description{
This function calculates indices of the Tau-U family as proposed by Parker et al. (2011).
}
\usage{
tauUSC(data, ties.method = "omit")
}

\arguments{
  \item{data}{
A single-case data frame.
}
  \item{ties.method}{
Defines how to handle ties. \code{"omit"} excludes all ties from the calculation. \code{"positive"} counts all ties as positive comparisons, while \code{"negative"} counts them as negative comparisons.
}
}

\value{
  \item{table}{Descriptives and comparisons from the Tau-U family.}
  \item{matrix}{The matrix used for calculating the indices.}
}
\references{
Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U. Behavior Therapy, 42, 284-299.  
}
\author{
Juergen Wilbert
}
\examples{
## Tau-U for the example from Parker et al. (2011)
bob <- makeSCDF(c(2, 3, 5, 3, 4, 5, 5, 7, 6), B.start = 5)
tauUSC(bob)

## Tau-U with ties counted as positive
tauUSC(Grosche2011$Eva)
}

