\name{describeSC}
\alias{describeSC}
\title{Descriptive statistics for single-case data}
\description{
The \code{describeSC} function provides common descriptive statistics for single-case data.
}

\usage{
describeSC(data)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{scdf}} to learn about this format.}
  
}

\value{A data frame of descriptive statistics (for each single-case), i.e.: number of observations, number of missing values, measures of central tendency, variation, and trend.}

\author{Juergen Wilbert}

\seealso{\code{\link{overlapSC}}, \code{\link{plotSC}}, \code{\link{writeSC}}}

\examples{
## Descriptive statistics for one single-case
describeSC(Waddell2011)

## Descriptive statistics for a list of three single-cases
describeSC(Grosche2011)

## Write descriptive statistics to .csv-file
study <- describeSC(Waddell2011)
write.csv(study$descriptives, file = "descriptives.csv")
}