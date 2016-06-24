\name{fillmissingSC}
\alias{fillmissingSC}
\title{Fill in missing measurement times}
\description{
The command fillmissingSC() fills in missing measurement times and estimated values if a single-case or multiple single-cases by linear interpolation. Optionally, missing values (NA) can also be replaced.
This procedure is recommended when you have larger gaps between measurement times or explicit missing values and want to calculate overlapping (PND, PAND, NAP ...) values or a randomization test.
}
\usage{
fillmissingSC(data, interpolation = "linear", na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{interpolation}{
Not in use yet.
}
  \item{na.rm}{
If set TRUE, NA values are also interpolated.
}
}

\author{Juergen Wilbert}


\examples{
#In his study, Grosche (2011) could not realize measurements each single week for 
#all participants. During the course of 100 weeks, about 20 measurements per person 
#at different times were administered. Here is a way to fill in the missing data:
new.data <- fillmissingSC(Grosche2011)
#Graphically compare the two datasets:
plotSC(c(Original = Grosche2011[2], Filled = new.data[2]))

set.seed(1234)
dat <- rSC(d.level = 1.0)
new.dat <- dat
replace.positions <- c(10,16,18)
new.dat[[1]][replace.positions,"values"] <- NA
filled.new.dat <- fillmissingSC(new.dat)
plotSC(c("original" = dat, "missing" = new.dat, "interpolated" = filled.new.dat), 
       marks = list(positions = replace.positions))

}

