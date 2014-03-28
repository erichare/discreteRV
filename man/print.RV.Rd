\name{print.RV}
\alias{print.RV}
\title{Print a random variable of class "RV"}
\usage{
\method{print}{RV}(x, odds = attr(x, "odds"), fractions = attr(x,
  "fractions"), ...)
}
\arguments{
  \item{x}{A random variable}

  \item{odds}{If TRUE, print as odds instead of probs}

  \item{fractions}{If TRUE, print probs as fractions
  instead of decimals}

  \item{...}{Additional arguments to be passed to the
  "print" function}
}
\description{
Print a random variable of class "RV"
}
\examples{
fair.die <- make.RV(1:6, rep(1/6,6))
print(fair.die)
}
\author{
Eric Hare \email{erichare@iastate.edu}
}

