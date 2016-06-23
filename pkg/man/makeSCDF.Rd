\name{makeSCDF}
\alias{makeSCDF}
\title{
Create a single case data frame
}
\description{
This procedure is used to create the basic data structure for storing a single or multiple single-cases. This basic data structure is used in all scan functions
}
\usage{
makeSCDF(data, B.start = NULL, MT = NULL)
}


\arguments{
  \item{data}{
A vector with measurements.
}
  \item{B.start}{
The start of the B phase.
}
  \item{MT}{
A vector defining measurement times. Defaut is MT = (1,2,3,...,n).
}
}

\value{
Returns a data frame formated as a single-case dataset. 
}

\author{
Juergen Wilbert
}

\examples{
person1 <- makeSCDF(c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19), B.start = 6)
plotSC(person1)

charlotte <- makeSCDF(c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19), 6)
theresa <- makeSCDF(c(3, 4, 3, 5, 7, 8, 7, 9, 8, 10, 12),5)
antonia <- makeSCDF(c(9, 8, 8, 7, 5, 7, 13, 14, 15, 12, 16), 7)
study1 <- list(charlotte, theresa, antonia)
names(study1) <- c("Charlotte", "Theresa", "Antonia")
describeSC(study1)

}

