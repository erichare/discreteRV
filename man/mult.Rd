\name{mult}
\alias{mult}
\title{Joint probability mass function of random variables X and Y}
\usage{
mult(X, Y, sep = ",", fractions = FALSE)
}
\arguments{
  \item{X}{random variable}

  \item{Y}{random variable}

  \item{sep}{separator between items from marginal
  distributions, by default set to ","}

  \item{fractions}{If TRUE, return the probabilities as
  fractions}
}
\description{
Joint probability mass function of random variables X and Y
}
\examples{
d <- make.RV(c("A","B","C"), odds = c(3,5,11))
d2 <- mult(d,d)
probs(d2)
}
\author{
Heike Hofmann \email{hofmann@iastate.edu}
}

