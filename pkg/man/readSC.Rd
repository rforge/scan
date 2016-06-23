\name{readSC}
\alias{readSC}
\title{Read single case datafiles}
\description{
Used for importing your data from a csv files.
}
\usage{
readSC(filename, sep = ",", dec = ".", sort.labels = FALSE, 
       phase.names = c("A", "B"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Name of the file to be imported.
}
  \item{sep}{
Decimal sign.
}
  \item{dec}{
Variable separator.
}
  \item{sort.labels}{
If set TRUE, the resulting list is sorted by label names (alphabetically, increasing).
}
  \item{phase.names}{
Name of the phases. Usually they should be "A" and "B". IF the datafile containes different names, these are converted to "A" and "B".
}
  \item{\dots}{
Further arguments passed to the read.table command.
}
}

\value{
Returns a scan Data structure
}
\author{Juergen Wilbert}
\examples{
#
}
