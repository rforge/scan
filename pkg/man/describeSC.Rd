\name{describeSC}
\alias{describeSC}
\title{
Descriptive statistics for single-case data
}
\description{
The describeSC function provides common descriptive statistics for single-case data.
}
\usage{
describeSC(data, decreasing = FALSE)
}

\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{decreasing}{
If you expect data to be lower in phase B, set \code{decreasing = TRUE}. At the moment describeSC only reports indices not sensible to decreasing phase B scores, so this does not change the output.
}
}
\value{
  \item{descriptives}{
A data frame of descriptive statistics.
}
}
\author{
Juergen Wilbert
}
\seealso{
\code{\link{writeSC}}.
}
\examples{
## descriptive statistics for one single-case
describeSC(Waddell2011)

## descriptive statistics for a list of three single-cases
describeSC(Grosche2011)

## write descriptive statistics to .csv-file
descr <- describeSC(Waddell2011)
write.csv(descr$descriptives, file = "descriptives.csv")

}

