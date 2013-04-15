\name{SofIID}
\alias{SofIID}
\title{Sum of independent identically distributed random variables}
\usage{
  SofIID(X, n = 2, digits = 15, scipen = 10)
}
\arguments{
  \item{X}{A random variable}

  \item{n}{The number of Xs to sum}

  \item{digits}{number of digits of precision used in the
  calculation. By defualt set to 15.}

  \item{scipen}{A penalty to be applied when deciding to
  print numeric values in fixed or exponential notation.
  Positive values bias towards fixed and negative towards
  scientific notation: fixed notation will be preferred
  unless it is more than scipen digits wider}
}
\description{
  Sum of independent identically distributed random
  variables
}
\examples{
X.Bern <- make.RV(c(1,0), c(.5,.5))

S5 <- SofIID(X.Bern, 5)
S128 <- SofIID(X.Bern, 128)
}

