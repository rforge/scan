\name{rciSC}
\alias{rciSC}
\alias{rCi}
\title{Reliable Change Index}
\description{
Computes different measures for a reliable change index (see Wise, 2004).
}
\usage{
rciSC(data, rel = 0.8, ci = 0.95, graph = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{rel}{
Reliability necessary for computing standard errors. Default is rel = 0.8.
}
  \item{ci}{
Span of the confidence interval (default is ci = 0.95 indicating a 95 percent confidence interval).
}
  \item{graph}{
If set TRUE, a boxplot with the mean of the A and B-phase and the confidence intervals is plotted (default is FALSE).
}
}
\references{
Wise, E. A. (2004). Methods for analyzing psychotherapy outcomes: A review of clinical significance, reliable change, and recommendations for future directions. Journal of Personality Assessment, 82(1), 50 - 59.
}
\author{
Juergen Wilbert
}

\examples{
rciSC(byHeart2011[1], graph = TRUE)
}
