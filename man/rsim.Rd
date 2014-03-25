\name{rsim}
\alias{rsim}
\title{Simulate n independent trials from a random variable X:}
\usage{
rsim(n, X)
}
\arguments{
  \item{n}{The number of independent trials to simulate}

  \item{X}{A random variable}
}
\description{
Simulate n independent trials from a random variable X:
}
\examples{
X.Bern <- make.RV(c(1,0), c(.5,.5))
X.Bern.sim100 <- rsim(100, X.Bern)

X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
X.loaded.die.sim100 <- rsim(100, X.loaded.die)

# The function 'rsim()' attaches the probabilities as names to the random draws.
# To get the values only, use 'as.vector()':
as.vector(X.Bern.sim100)
as.vector(X.loaded.die.sim100)
}

