\name{overlapSC}
\alias{overlapSC}
\title{
Overlap for single-case data
}
\description{
The overlapSC function provides an overview of the most common overlap indices.
}
\usage{
overlapSC(data, decreasing = FALSE)
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
  \item{overlap}{A data frame consisting of the following indices for each single-case as well as the average for all cases}
  \item{PND}{Percent non-overlapping data}
  \item{PEM}{Percent exceeding the median}
  \item{PET}{Percent exceeding the trend}
  \item{NAP}{Nonoverlap of all pairs}
  \item{PAND}{Percent all non-overlapping data}
  \item{TAU.U}{Tau-U}
}
\author{
Juergen Wilbert
}
\examples{
set.seed(1235)
dat <- rSC(8, B.start = round(runif(5,5,10)), d.level = 0.5, round = 0, random.names = TRUE)
overlapSC(dat)

}

