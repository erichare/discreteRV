\name{probs}
\alias{probs}
\title{Probability mass function of random variable X}
\usage{
  probs(X, scipen = 10, digits = 22)
}
\arguments{
  \item{X}{random variable}

  \item{digits}{number of digits of precision used in the
  calculation. By defualt set to 15.}

  \item{scipen}{A penalty to be applied when deciding to
  print numeric values in fixed or exponential notation.
  Positive values bias towards fixed and negative towards
  scientific notation: fixed notation will be preferred
  unless it is more than scipen digits wider}
}
\value{
  named vector of probablities for each element of the
  random variable
}
\description{
  Obtain the list of probabilities from a random variable:
  p(x)
}
\examples{
X.Bern <- make.RV(c(1,0), c(.5,.5))
probs(X.Bern)

X.fair.die <- make.RV(1:6, rep("1/6",6))
probs(X.fair.die)

X.loaded.die <- make.RV(1:6, c(1,1,1,1,2,4))
probs(X.loaded.die)
}

