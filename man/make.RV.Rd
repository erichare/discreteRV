\docType{package}
\name{make.RV}
\alias{make.RV}
\alias{make.RV-package}
\title{MakeRV}
\usage{
  make.RV(vals, probs.or.odds)
}
\arguments{
  \item{vals}{vector of possible outcomes}

  \item{probs.or.odds}{vector of probabilities or odds.}
}
\value{
  random variable as RV object.
}
\description{
  Make a random variable consisting of possible outcome
  values and their probabilities or odds
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
}

