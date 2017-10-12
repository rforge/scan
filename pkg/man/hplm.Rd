\name{hplm}
\alias{hplm}
\title{Hierarchical piecewise linear model / piecewise regression}
\description{
The \code{hplm} function computes a hierarchical piecewise regression model.
}

\usage{
hplm(data, model = "B&L-B", method = "ML", control = list(opt = "optim"),
  random.slopes = FALSE, ICC = TRUE, phases = c("A", "B"),
  formula = values ~ 1 + mt + phase + inter, data.l2 = NULL)
}

\arguments{
  \item{data}{A single-case data frame. See \code{\link{scdf}} to learn about this format.}
  \item{model}{Model used for calculating the slope parameter (see Huitema & McKean, 2000). Default is \code{model = "B&L-B"}. Possible values are: \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"}, and \code{"Manly"}.}
  \item{method}{Method used to fit your model. Pass \code{"REML"} to maximize the restricted log-likelihood or \code{"ML"} for maximized log-likelihood. Default is \code{"ML"}.}
  \item{control}{A list of settings for the estimation algorithm, replacing the default values passed to the function \code{lmeControl} of the \code{nlme} package.}
  \item{random.slopes}{If \code{random.slopes = TRUE} random slope effects of the level, trend, and treatment parameter are estimated.}
\item{ICC}{If \code{ICC = TRUE} an intraclass-correlation is estimated.}
\item{formula}{Defaults to the standard piecewise regression model. The parameter 'inter' adresses the intercation term defined based on the method provide in the model argument (e.g., "B&L-B"). The formula can be changed for example to include further L1 or L2 variables into the regression model.}
\item{data.l2}{A dataframe providing additional variables at Level 2. The scdf File has to have names for all cases and the Level 2 dataframe has to have a column named 'cases' with the names of the cases the Level 2 variables belong to.}

  \item{phases}{
A vector of two characters or numbers indicating the two phases that should be compared. E.g., \code{phases = c("A","C")} or  \code{phases = c(2,4)} for comparing the second to the fourth phase. Phases could be combined by providing a list with two elements. E.g., \code{phases = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2 and 4 (as B). Default is \code{phases = c("A","B")}.
}

}

\value{
  \item{model}{Character string from function call (see \code{Arguments} above).}
  \item{method}{Character string from function call (see \code{Arguments} above).}
  \item{N}{Number of single-cases.}
  \item{analyze.random.slopes}{Logical argument from function call (see \code{Arguments} above).}
  \item{analyze.ICC}{Logical argument from function call (see \code{Arguments} above).}
  \item{random.trend.level}{Linear mixed-effects model with random trend and level effect.}
  \item{random.trend.slope}{Linear mixed-effects model with random trend and slope effect.}
  \item{random.level.slope}{Linear mixed-effects model with random level and slope effect.}
  \item{random.trend.level.slope}{Linear mixed-effects model with random trend, level, and slope effect.}
  \item{random.nointercept.trend.level.slope}{Linear mixed-effects model with random trend, level, and slope effect without intercept.}
  \item{random.trend}{Significance test for random trend effect.}
  \item{random.level}{Significance test for random level effect.}
  \item{random.slope}{Significance test for random slope effect.}
  \item{ICC}{Intraclass correlation.}
  \item{L.ICC}{L ratio from intraclass correlation.}
  \item{p.ICC}{P-Value for intraclass correlation.}
}

\author{Juergen Wilbert}

\seealso{\code{\link{plm}}}

\examples{
## Compute hplm model on a MBD over three cases (restricted log-likelihood)
dat <- rSC(3, MT = 30, B.start = 11, d.level = 1.0, d.slope = 0.05, d.trend = 0.05)
hplm(dat, method = "REML")

## Use hplm with default settings
dat <- rSC(15, MT = 30, B.start = 11, d.level = 1.0, d.slope = runif(15,0,0.2), d.trend = 0.05)
hplm(dat)
}

\concept{Segmented Regression}