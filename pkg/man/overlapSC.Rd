\name{overlapSC}
\alias{overlapSC}
\title{
Overlap indices for single-case data
}
\description{
The \code{overlapSC} function provides the most common overlap indices for single-case data and some additional statistics.
}
\usage{
overlapSC(data, decreasing = FALSE, phases = c("A","B"))
}
\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{decreasing}{
If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
}
  \item{phases}{
A vector of two characters or numbers indicating the two phases that should be compared. E.g., \code{phases = c("A","C")} or  \code{phases = c(2,4)} for comparing the second to the fourth phase. Default is \code{phases = c("A","B")}.
}
}
\value{
  \item{overlap}{A data frame consisting of the following indices for each single-case for all cases: PND, PEM, PET, NAP, PAND, Tau-U (A vs. B - Trend A), Diff_mean, Diff_trend, SMD.}
}
\author{
Juergen Wilbert
}
\examples{
## Display overlap indices for one single-case
overlapSC(Huitema2000, decreasing = TRUE)

## Display overlap indices for a list of six single-cases
overlapSC(GruenkeWilbert2014)

## Write overlap indices to .csv-file
overl <- overlapSC(Waddell2011)
write.csv(overl$overlap, file = "overlap_indices.csv")
}

\concept{Overlap}