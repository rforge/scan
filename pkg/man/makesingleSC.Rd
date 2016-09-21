\name{makesingleSC}
\alias{makesingleSC}
\title{Aggregate multiple single-cases into one case.}
\description{
The makesingleSC function combines multiple single-case data frames into one single-case data frame.
}
\usage{
makesingleSC(data, scale = FALSE, type = "add")
}
\arguments{
  \item{data}{
A vector with measurements, a data frame or a list of data frames.
}
  \item{scale}{Unused}
  \item{type}{By default values with the same measurement are added. If type is set to "mean" or "median", values of the same measurement are replaced with their mean or median. Default is "add".
}
}
\details{
The algorithm works the following way:
1)  All measurements of each single-case are mean-centred with respect to the mean of the A-phase of each single-case.
2)  The values of the A-phases of all single-cases are combined and ordered ascending with respect to the measurement times.
3)  The values of the B-phases are combined and ordered as well.
4)	The measurements of the B-phases are attached behind the A-phase measurements. The measurement times of the B-phase are shifted to start one measurement time after the A-phase.
}

\author{
Juergen Wilbert
}
\examples{
study1 <- rSC(3, d.level = 0.8, d.slope = 0.1, round = 0)
new.sc <- makesingleSC(study1)
plotSC(new.sc, type = "p", lines = "trend")
}