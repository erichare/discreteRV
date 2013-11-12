\name{multN}
\alias{multN}
\title{Probability mass function of  X^n}
\usage{
  multN(X, n = 2, digits = 30, scipen = 20, sep = ".",
    fractions = FALSE)
}
\arguments{
  \item{X}{random variable}

  \item{n}{power}

  \item{digits}{number of digits of precision used in the
  calculation. By default set to 15.}

  \item{scipen}{A penalty to be applied when deciding to
  print numeric values in fixed or exponential notation.
  Positive values bias towards fixed and negative towards
  scientific notation: fixed notation will be preferred
  unless it is more than scipen digits wider}

  \item{sep}{separator between items from marginal
  distributions, by default set to "."}

  \item{fractions}{If TRUE, return the probabilities as
  fractions}
}
\description{
  Probability mass function of X^n
}
\examples{
d <- make.RV(c("A","B","C"), c(3,5,11))
d2 <- multN(d)
probs(d2)
}
\author{
  Heike Hofmann \email{hofmann@iastate.edu}
}

