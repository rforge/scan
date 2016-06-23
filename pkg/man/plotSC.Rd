\name{plotSC}
\alias{plotSC}
\title{Plot AB-designs}
\description{
The plotSC command draws a nice plot of one or multiple single-cases. By default, plotSC draws a multiple single-case plot if the given data contains more than one case.
}
\usage{
plotSC(data, ylim = NULL, xlim = NULL, fill = "", lines = "", marks = NULL, 
       annotations = NULL, phase.names = c("A", "B"), FUN.AB = NULL, 
       xlab = "Measurement time", ylab = "Score", text.ABlag = NULL, lwd = 2, 
       pch = 17, type = "b", mai = c(0.6, 0.82, 0.2, 0.42), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{ylim}{
Lower and upper limit for the y-axis of the plot (e.g., ylim = c(0, 20) sets the y-axis to a scale from 0 to 20). In multiple single cases plots you can use ylim = c(0, NA) to scales the y-axis from 0 to the maximum of each case. If the ylim parameter is not set, scan automatically sets a proper scale. 
}
  \item{xlim}{
Lower and upper limit for the x-axis of the plot. If the xlim parameter is not set, scan automatically computers a proper scale. 
}
  \item{fill}{
  If set, the area beneath the plot line is coloured in the given colour (e.g., fill = "grey"). Use the standard R command colours() to get a list of possible colours.
}
  \item{lines}{
A character or list defining one or more lines or curves to be plotted for a better estimation of statistical parameters. The argument is either given as a single character string lines = "median" or a list of character strings lines = list("median", "trend"). Additionally, some of the procedures can be refined by a single argument e.g., lines = list("mean" = 0.10) adds a 10 percent trimmed mean line. See details.
}

  \item{marks}{
A list of parameters defining the marking of certain data points.

"positions" = A vector or a list of vectors indicating measurement-times to be marked. In case of a vector, the marked measurement-times are the same for each plotted case. In case of a list of vectors, marks are set differently for each case. The list must have the same length as there are cases in the datafile. 

"col" = A character string. The colour of the marks.

"cex" = The size of the marks.
}
  \item{annotations}{
A list defnining several parameters for plotting annotations to each data point. col = colour of the annotations, cex = size of the annotations, pos = position of plotting annotations (1=below, 2=left, 3=above, 4=right), round = rounds the values of the annotations to the specified number of decimal places.

}
  \item{phase.names}{
scan uses the default names 'A' and 'B' for the two experimental phases. You can change these with the phase.names parameter. Example: phase.names = c("Baseline", "Treatment").
}

 \item{FUN.AB}{
Unused.
}
  \item{xlab}{
The label of the x-axis.
}
  \item{ylab}{
The labels of the y-axis.
}
  \item{text.ABlag}{
By default, scan draws a vertical line to separate the A and B-phase. Alternatively, you could print a character string between the two phases using the textAB.lag parameter (e.g., textAB.lag = "Start").
}
  \item{lwd}{
Width of the plot lines. Default is 2.
}
  \item{pch}{
Point type. Default is 17. (e.g., pch = 16 draws filled circles; pch = "A" draws As).
}
  \item{type}{
Sets the plot type. "l" draws lines, "p" points, "b" draws lines and points, and "n" draws nothing onto the plot (default of plotSC is "b"). The "n" argument can be useful in combination with the lines argument (e.g., type = "n", lines = "loreg").
}
  \item{mai}{
Sets the margins of the plot.
}

  \item{\dots}{
Further arguments for passing to the plot command.
}
}
\details{
Paramters for the argument "lines".

"median"  A median line for the A and B-phase.

"mean"	A mean line for the A and B-phase. Default is a 10 percent trimming. Other trimming values can be set by a second parameter e.g., lines = list(mean = 0.2) draws a 20 percent trimmed mean line.

"trend"	A trend line for the A and B-phase.

"trendA"  A trend line for the A-phase.

"maxA"  A line for the maximum of the A-phase.

"medianA" A line for the median of the A-phase.

"meanA" A line for the 10 percent trimmed mean of the A-phase. Trimming can be changed in the following way lines = list(meanA = 0.15).

"plm" A regression line for a piecewise linear regression model.

"plm.ar"  A regression line for a piecewise autoregression model. The lag is specified by plm.ar = 2. Where the number is the maximum order of the autoregression analysis. 

"movingMean"  Draws a moving mean curve. The lag can be specified by lines = list(movingMean = 2). Default is a lag 1 curve.

"movingMedian"  Draws a moving median curve. The lag can be specified by lines = list(movingMedian = 3). Default is a lag 1 curve.

"loreg" A non-parametric local regression line. The proportion of data influencing each data point can be specified by lines = list("loreg" = 0.66). The default is 0.5.

"lty", "lwd", "col" Specifies line type ltw, line width lwd, and line colour col. Default is a black dashed line with the width 2. E.g., to draw a solid red thick mean line type lines = list("mean", lty = "solid", lwd = 4, col = "red"). Some of the possible line types are "solid", "dashed", and "dotted".
}

\author{
Juergen Wilbert
}

\examples{
plotSC(Grosche2011, fill = "grey", type = "n", ylim = c(0,NA), 
       xlab = "Training session", ylab = "Words per minute",
       phase.names = c("Baseline", "Intervention"), 
       lines = list("plm", lty = "solid", col = "black", lwd = 3))

set.seed(1234)
dat <- rSC(3)
plotSC(dat, marks = list(positions = list(c(2,4,5),c(1,2,3),c(7,8,9)), col = "blue",
       cex = 1.4),annotations = list(label = "values","col" = "red", cex = 0.75,
       offset = 1, round = 0))
}

