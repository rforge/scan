\name{pnd}
\alias{pnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percent non-overlapping data

}
\description{
The function pnd return the percentage non-overlapping data. If you expect a decrease in the B-phase, set the parameter decreasing = TRUE. Due to its error-proneness you should not use PND but NAP or PAND instead (see Parker & Vannest, 2009).
}
\usage{
pnd(data, decreasing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{decreasing}{
If you expect the data to be lower in the B phase, set decreasing = TRUE (default is FALSE).
}

}
\author{
Juergen Wilbert
}
\examples{
#
}

