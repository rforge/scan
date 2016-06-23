\name{plm}
\alias{plm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Piecewise linear model/Piecewise Regression
}
\description{
The plm function computes a piecewise regression model (see Huitema & Mckean, 2000). A piecewise regression models a linear trend in the data, a level and a slope increase with the start of the intervention. Additionally, the plm function computes two effect-sizes by comparing restricted to full regression models (see Beretvas & Chung, 2008). Furthermore, an autoregression model can be used to take into account autocorrelated data (Beretvas & Chung, 2008). For the latter, your R system must comprise an installation of the nlme package.
}
\usage{
plm(data, AR = NULL, model = "B&L-B", count.data = FALSE, 
    family = ifelse(count.data, "poisson", "gaussian"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{AR}{
Autoregression modelled up to the given lag based on the ARMA function.
}
  \item{model}{
Regression model used for computation (see Huitema & Mckean, 2000). Default is model = "B&L-B". Possible values are: B&L-B, H-M, Mohr#1, Mohr#2, Manly.
}
  \item{count.data}{
%%     ~~Describe \code{count.data} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{...}{
%%     ~~Describe \code{family} here~~
}

}
\references{
Beretvas, S., & Chung, H. (2008). An evaluation of modified R2-change effect size indices for single-subject experimental designs. Evidence-Based Communication Assessment and Intervention, 2, 120-128.

Huitema, B. E., & Mckean, J. W. (2000). Design specification issues in time-series intervention models. Educational and Psychological Measurement, 60(1), 38-58.
}
\author{
Juergen Wilbert
}
\examples{
set.seed(1000)
dat <- rSC(1, MT = 30, B.start = 11, d.level = 1.0, d.slope = 0.05, d.trend = 0.05)
plm(dat, AR = 3)

}

