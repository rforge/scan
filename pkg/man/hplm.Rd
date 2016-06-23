\name{hplm}
\alias{hplm}
\title{ 
Hierarchical Piecewise linear model/Piecewise Regression
}
\description{
The hplm function computes a hierarchical piecewise regression model.
}
\usage{
hplm(data, model = "B&L-B", method = "ML", control = list(opt = "optim"), 
     random.slopes = TRUE, ICC = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{model}{
Regression model used for computation (see Huitema & Mckean, 2000). Default is model = "B&L-B". Possible values are: B&L-B, H-M, Mohr#1, Mohr#2, Manly.
}

  \item{method}{
a character string. If "REML" the model is fit by maximizing the restricted log-likelihood. If "ML" the log-likelihood is maximized. Defaults to "ML".
}


\item{control}{
a list of control values for the estimation algorithm to replace the default values returned by the function lmeControl of the nlme package.
}

  \item{random.slopes}{
If TRUE random slope effects of the level, trend, and treatment parameter are estimated.
}

\item{ICC}{
If TRUE an intraclass-correlation is estimated.
}


}
\author{
Juergen Wilbert
}
\examples{
set.seed(1000)

dat <- rSC(3, MT = 30, B.start = 11, d.level = 1.0, d.slope = 0.05, d.trend = 0.05)
hplm(dat, method = "REML",random.slopes = FALSE)

set.seed(1000)

dat <- rSC(15, MT = 30, B.start = 11, d.level = 1.0, d.slope = runif(15,0,0.2), d.trend = 0.05)
hplm(dat)


}

