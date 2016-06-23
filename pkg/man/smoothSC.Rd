\name{smoothSC}
\alias{smoothSC}
\title{Smoothing}
\description{
This command provides three different procedures to smooth the data (i.e., to eliminate noise). 
A moving average function (either mean based or median based) replaces step-by-step each data point by the average of the surrounding data. With a local regression function, each data point is regressed by its surrounding values. 
}
\usage{
smoothSC(data, FUN = "movingMedian", intensity = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{FUN}{
Function to compute the smoothing. Default is a lag 1 moving Median functioon (see details).
}
  \item{intensity}{
In the case of movingMean or movingMedian it is the lag (i.e., the range of values around the target value) that is used for computing the average (default is 1). In case of localRegression it is the proportion of data influencing each data point (default is 0.2).
}
}
\details{
Values for the FUN arguments.

"movingMean" Computes the moving mean.

"movingMedian" Computes the moving median.

"localRegression" Computes non-parametric local regressions.

}
\author{
Juergen Wilbert
}
\examples{
new.dat <- smoothSC(Huber2014$Berta)
new.dat2 <- smoothSC(Huber2014$Berta, FUN = "movingMean")
new.dat3 <- smoothSC(Huber2014$Berta, FUN = "localRegression")
plotSC(list("Original" = Huber2014$Berta,"Moving Median" = new.dat[[1]],
            "Moving Mean" = new.dat2[[1]],"Local Regression" = new.dat3[[1]]))
}

