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
fair.die <- make.RV(1:6, rep("1/6",6))
SofIID(fair.die, n=3)
}

