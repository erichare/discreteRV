\name{SofI}
\alias{SofI}
\title{Sum of independent random variables}
\usage{
  SofI(..., digits = 15, scipen = 10)
}
\arguments{
  \item{...}{Arbitrary number of random variables}

  \item{digits}{number of digits of precision used in the
  calculation. By defualt set to 15.}

  \item{scipen}{A penalty to be applied when deciding to
  print numeric values in fixed or exponential notation.
  Positive values bias towards fixed and negative towards
  scientific notation: fixed notation will be preferred
  unless it is more than scipen digits wider}
}
\description{
  Sum of independent random variables
}
\examples{
X.Bern <- make.RV(c(1,0), c(.5,.5))
X.fair.die <- make.RV(1:6, rep("1/6",6))

S5 <- SofI(X.Bern, X.Bern, X.Bern, X.Bern, X.Bern)
S.mix <- SofI(X.Bern, X.fair.die)  # Independent but not IID
}

