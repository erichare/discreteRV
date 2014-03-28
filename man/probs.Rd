\name{probs}
\alias{probs}
\title{Probability mass function of random variable X}
\usage{
probs(X)
}
\arguments{
  \item{X}{random variable}
}
\value{
named vector of probablities for each element of the random
variable
}
\description{
Obtain the list of probabilities from a random variable:
p(x)
}
\examples{
X.Bern <- make.RV(c(1,0), c(.5,.5))
probs(X.Bern)

X.fair.die <- make.RV(1:6, rep(1/6,6))
probs(X.fair.die)

X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
probs(X.loaded.die)
}

