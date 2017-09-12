\name{plm}
\alias{plm}
\alias{newplm}
\title{Piecewise linear model / piecewise regression}
\description{
The \code{plm} function computes a piecewise regression model (see Huitema & McKean, 2000).
}

\usage{
plm(data, AR = 0, model = "B&L-B", phases = c("A", "B"), family = "gaussian", 
  formula = values ~ 1 + mt + phase + inter, na.action = na.omit, ...)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{AR}{Maximal lag of autoregression. Modeled based on the Autoregressive-Moving Average (ARMA) function.  When AR is set, the family argument must be set to  \code{family = "gaussian"}.}
  \item{model}{Regression model used for computation (see Huitema & McKean, 2000). Default is \code{model = "B&L-B"}. Possible values are: \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"}, and \code{"Manly"}.}
  \item{family}{Set the distributioin family. Defaults to a gaussian distribution. See the \code{family} function for more details.}
  \item{phases}{-}
  \item{formula}{-}
  \item{...}{Further arguments passed to the glm function.}
}

\value{
  \item{model}{Character string from function call (see \code{Arguments} above).}
  \item{F}{F value for specified model.}
  \item{df1}{Degrees of freedom (Regression).}
  \item{df2}{Degrees of freedom (Residual).}
  \item{p}{P value for specified model.}
  \item{R2}{Explained variance R squared.}
  \item{R2.adj}{Adjusted R squared.}
  \item{count.data}{Logical argument from function call (see \code{Arguments} above).}
  \item{ES.slope}{Effect size / Explained variance gain of slope.}
  \item{ES.trend}{Effect size / Explained variance gain of trend.}
  \item{full.model}{Full regression model list (including \code{coefficients}, \code{residuals} and many others}
  \item{MT}{Number of measurements.}
  \item{data}{Single-case data frame passed to the function.}
  \item{N}{Number of single-cases.}
  \item{family}{Character string from function call (see \code{Arguments} above).}
}

\references{
Beretvas, S., & Chung, H. (2008). An evaluation of modified R2-change effect size indices for single-subject experimental designs. \emph{Evidence-Based Communication Assessment and Intervention, 2}, 120-128.

Huitema, B. E., & McKean, J. W. (2000). Design specification issues in time-series intervention models. \emph{Educational and Psychological Measurement, 60}, 38-58.
}

\author{Juergen Wilbert}

\seealso{\code{\link{hplm}}}

\examples{
## Compute a piecewise regression model for a random single-case
dat <- rSC(1, MT = 30, B.start = 11, d.level = 1.0, d.slope = 0.05, d.trend = 0.05)
plm(dat, AR = 3)
}

\concept{Segmented Regression}