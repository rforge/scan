\name{pand}
\alias{pand}

\title{
Percent all non-overlapping data
}
\description{
The percent non-overlapping data is a measurement for estimating a level increase (or decrease) in performance after an intervention. It can be computed with the pand command. PAND allows for a Chi-square statistical test (comparing real and estimated association a data points with phase A and B) and thereby allows an estimation of the effect size phi, which is identical to Pearsons r computed with dichotomous data. Phi-Square is thus the amount of explained variance. The original procedure for computing PAND as proposed by Parker and colleagues (Parker u. a., 2007) does not take ambivalent data points due to ties into account, as does the computation for the nonoverlap of all pairs index (Parker & Vannest, 2009). In the pand procedure, the frequency matrix is corrected for ties.
}
\usage{
pand(data, decreasing = FALSE, correction = TRUE)
}
 
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{decreasing}{
If you expect the data to be lower in the B phase, set decreasing = TRUE (default is FALSE).
}
  \item{correction}{
If set TRUE, the frequency matrix is corrected for ties. A tie is counted as a half-measuremnt in each phase.  
}
}

\references{
Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage of All Non-Overlapping Data (PAND) An Alternative to PND. The Journal of Special Education, 40(4), 194 - 204.
Parker, R. I., & Vannest, K. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. Behavior Therapy, 40(4), 357 - 367.
}
\author{
Juergen Wilbert
}


\examples{
set.seed(1234)
dat <- rSC(1, d.level = 1.0)
pand(dat)

# PAND when expecting a decrease in performance 
# after intervention in a multiple-baseline design:
set.seed(1234)
dat <- rSC(3, B.start = c(5, 7, 9), d.level = -1.0)
pand(dat, decreasing = TRUE)
}

