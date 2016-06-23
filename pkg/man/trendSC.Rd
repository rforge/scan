\name{trendSC}
\alias{trendSC}
\title{Trends in single Cases}
\description{
If you want to get an overview of linear trends in the different phases of a single-case you can use the trendSC command. By default, it gives you the intercept and slope of a linear and a squared regression model of the measurement-time on the values. Models are computed for the A phase, the B-phase (with correcting the measurement-time to start at one), and the combined A and B-Phase. For a more advanced application, you can add further regression models using the R specific formula class, which will be applied again to the A-phase, the B-phase, and the combined A and B-phase.
}
\usage{
trendSC(data, B.offset = -1, model = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{B.offset}{
An offset for the first measurement-time (MT) of the B-phase. If set 0, the first MT of the B-phase is set to 1. The default is -1, so the MT of the B-phase begin with 0.
}
  \item{model}{
A string or a chain of (named) strings each depicting one regression model. This is a formula expression of the standard R class. The parameters of the model are values, mt and phase. 
}
}
\author{
Juergen Wilbert
}
\examples{
set.seed(1000)
dat <- rSC(1, d.slope = 0.5)
trendSC(dat)

#In addition to the linear and squared regression model, two further models are computed:
#a) a cubic model, and b) the values predicted by the natural 
#logarithm of the measurement time. Additionally, the data of eight 
#single-cases are aggregated.
set.seed(1000)
dat <- rSC(1, d.slope = 0.3)
trendSC(dat, B.offset = 0, model = c("Cubic" = "values ~ I(mt^3)",
        "Log Time" = "values ~ log(mt)"))

}

