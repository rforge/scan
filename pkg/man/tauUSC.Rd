\name{tauUSC}
\alias{tauUSC}
\title{Tau-U}
\description{
The tau-U index was proposed by Parker and colleagues.

}
\usage{
tauUSC(data, ties.method = "omit")
}

\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{ties.method}{
Defines how to handle ties. 'omit' does not include ties into the calculatioin of tau-u, 'positive' and 'negative' are two further possibilities. 
}

}
\details{
#
}
\value{
  \item{table}{The differnt comparisons for estimating the tau-U effect. Tau-U can be derived as the tau value of the A vs. B + trend B - trend A comparison.}
  \item{matrix}{The matrix for calculating tau-U}
}
\references{
Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U. Behavior Therapy, 42, 284-299.  
}
\author{
Juergen Wilbert
}
\examples{
#example of Parker et al. 2011
exa <- makeSCDF(c(2, 3, 5, 3, 4, 5, 5, 7, 6),5)
tauUSC(exa)
}

