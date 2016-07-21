\name{overlapSC}
\alias{overlapSC}
\title{
Overlap indices for single-case data
}
\description{
The overlapSC function provides the most common overlap indices for single-case data.
}
\usage{
overlapSC(data, decreasing = FALSE)
}

\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{decreasing}{
If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
}
}
\value{
  \item{overlap}{A data frame consisting of the following indices for each single-case as well as the average for all cases}
  \item{PND}{Percent non-overlapping data}
  \item{PEM}{Percent exceeding the median}
  \item{PET}{Percent exceeding the trend}
  \item{NAP}{Nonoverlap of all pairs}
  \item{PAND}{Percent all non-overlapping data}
  \item{TAU.U}{Tau-U (A vs. B + Trend B - Trend A)}
}
\author{
Juergen Wilbert
}
\examples{
## overlap indices for one single-case
overlapSC(Huitema2000, decreasing = TRUE)

## descriptive statistics for a list of six single-cases
describeSC(GruenkeWilbert2014)

## write overlap indices to .csv-file
overl <- overlapSC(Waddell2011)
write.csv(overl$overlap, file = "overlap_indices.csv")
}

