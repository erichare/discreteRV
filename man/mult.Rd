\name{mult}
\alias{mult}
\title{Joint probability mass function of random variables X and Y}
\usage{
  mult(X, Y, digits = 15, scipen = 10, sep = ".")
}
\arguments{
  \item{X}{random variable}

  \item{Y}{random variable}

  \item{digits}{number of digits of precision used in the
  calculation. By default set to 15.}

  \item{scipen}{A penalty to be applied when deciding to
  print numeric values in fixed or exponential notation.
  Positive values bias towards fixed and negative towards
  scientific notation: fixed notation will be preferred
  unless it is more than scipen digits wider}

  \item{sep}{separator between items from marginal
  distributions, by default set to "."}
}
\description{
  Joint probability mass function of random variables X and
  Y
}
\examples{
d <- make.RV(c("A","B","C"), c(3,5,11))
d2 <- mult(d,d)
probs(d2)
}

