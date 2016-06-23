\name{truncateSC}
\alias{truncateSC}
\title{
Truncating data
}
\description{
This function is used to skip some measuremnts at the beginning and/or end of each phase.
}
\usage{
truncateSC(data, A = c(0,0), B = c(0,0))
}
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{A}{
A vector of two values. The first sets the number of measurements at the beginning of the A phase that are truncated and the second the umber of mesearments at the end of the A phase. In the case of multiple baseline datasets, the truncation is adapted to the length of the A phase for each single case.
}
  \item{B}{
A vector of two values. The first sets the number of measurements at the beginning of the B phase that are truncated and the second the umber of mesearments at the end of the B phase. In the case of multiple baseline datasets, the truncation is adapted to the length of the B phase for each single case.
}

}
\author{
Juergen Wilbert
}
\examples{
# truncate the first two measurements of the A and B phase
newData <- truncateSC(byHeart2011[1], A = c(2,0), B = c(2,0))
plotSC(c(original = byHeart2011[1], selected = newData))
}
