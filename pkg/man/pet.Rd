\name{pet}
\alias{pet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percent exceeding the trend
}
\description{
The function pet return the percentage of the B-phase data exceeding the predicted values on basis of the trend of the A-phase. A binomial test against a 50/50 distribution is computed. Furthermore, the percent of data points in the B-phase exceeding the upper (or lower) threshold of the 95 percent confidence interval of the predicted values is returned. If you expect a decrease in the B-phase, set the parameter decreasing = TRUE.
}
\usage{
pet(data, ci = 0.95, decreasing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{decreasing}{
If you expect the data to be lower in the B phase, set decreasing = TRUE (default is FALSE).
}
  \item{ci}{
The size of the confidence interval (by default ci = .95).
}

}
\author{
Juergen Wilbert
}
\examples{
set.seed(1234)
dat <- rSC(5, d.slope = 0.2)
pet(dat)

}

