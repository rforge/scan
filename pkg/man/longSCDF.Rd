\name{longSCDF}
\alias{longSCDF}
\title{Create a single data frame from several single-case data frames.
}
\description{
The longSCDF function transposes a list of single-case data frames (wide) into one (long) data frame.
}
\usage{
longSCDF(data)
}

\arguments{
  \item{data}{
A list of single-case data frames
}
}
\value{
Returns one data frame with data from all single-cases structured by the variable 'case'. 
}
\author{
Juergen Wilbert
}
\examples{
## Convert the list of three single-case data frames from Grosche (2011) into one long data frame
Grosche2011
Grosche2011_long <- longSCDF(Grosche2011)
Grosche2011_long
}