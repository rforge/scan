\name{hplm}
\alias{hplm}
\title{Hierarchical piecewise linear model / piecewise regression}
\description{
The \code{hplm} function computes a hierarchical piecewise regression model.
}

\usage{
hplm(data, model = "B&L-B", method = "ML", control = list(opt = "optim"),
  random.slopes = TRUE, ICC = TRUE)
}

\arguments{
  \item{data}{A single-case data frame. See \code{\link{makeSCDF}} to learn about this format.}
  \item{model}{Regression model used for computation (see Huitema & McKean, 2000). Default is \code{model = "B&L-B"}. Possible values are: \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"}, and \code{"Manly"}.}
  \item{method}{Method used to fit your model. Pass \code{"REML"} to maximize the restricted log-likelihood or \code{"ML"} for maximized log-likelihood. Default is \code{"ML"}.}
  \item{control}{A list of settings for the estimation algorithm, replacing the default values passed to the function \code{lmeControl} of the \code{nlme} package.}
  \item{random.slopes}{If \code{random.slopes = TRUE} random slope effects of the level, trend, and treatment parameter are estimated.}
\item{ICC}{If \code{ICC = TRUE} an intraclass-correlation is estimated.}
}

\value{
  \item{model}{Character string from function call (see \code{Arguments} above).}
  \item{method}{Character string from function call (see \code{Arguments} above).}
  \item{N}{Number of single-cases.}
  \item{analyze.random.slopes}{Logical argument from function call (see \code{Arguments} above).}
  \item{analyze.ICC}{Logical argument from function call (see \code{Arguments} above).}
  \item{random.trend.level}{}
  \item{random.trend.slope}{}
  \item{random.level.slope}{}
  \item{random.trend.level.slope}{}
  \item{random.nointercept.trend.level.slope}{}
  \item{random.trend}{}
  \item{random.level}{}
  \item{random.slope}{}
  \item{model.0}{}
  \item{ICC}{}
  \item{model.without}{}
  \item{L.ICC}{}
  \item{p.ICC}{}
}

\author{Juergen Wilbert}

\seealso{\code{\link{plm}}}

\examples{
## Compute hpl model on a MBD over three cases (restricted log-likelihood)
dat <- rSC(3, MT = 30, B.start = 11, d.level = 1.0, d.slope = 0.05, d.trend = 0.05)
hplm(dat, method = "REML",random.slopes = FALSE)

## Use hplm with default settings
dat <- rSC(15, MT = 30, B.start = 11, d.level = 1.0, d.slope = runif(15,0,0.2), d.trend = 0.05)
hplm(dat)
}

\concept{Segmented Regression}