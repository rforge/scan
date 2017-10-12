\name{longSCDF}
\alias{longSCDF}
\title{Creating a long format data framefrom several single-case data frames (scdf).
}
\description{
The longSCDF function transposes a scdf into one long data frame. Additionally, a data frame can be merged that includes data of the subjects. 
}

\usage{
longSCDF(data, l2 = NULL, id = "case", model = NULL, ...)
}

\arguments{
  \item{data}{
A single-case data frame (scdf).
}
\item{l2}{A dataframe providing additional variables at Level 2. The scdf has to have names for all cases and the Level 2 dataframe has to have a column named 'cases' corresponding case names.}
\item{id}{Variable name of the Level 2 data frame that contains the case names.}
\item{model}{If set, calculates interaction terms (see 'method' argument in plm function).}
\item{...}{Additional arguments provided to the internally used merge function.}
}

\value{
Returns one data frame with data of all single-cases structured by the variable 'case'. 
}

\author{Juergen Wilbert}

\seealso{\code{\link{makeSCDF}}, \code{\link{makesingleSC}}, \code{\link{writeSC}}}

\examples{
## Convert the list of three single-case data frames from Grosche (2011) into one long data frame
Grosche2011
Grosche2011_long <- longSCDF(Grosche2011)
Grosche2011_long
}

\keyword{manip}