\name{KURT}
\alias{KURT}
\title{Kurtosis of a random variable}
\usage{
  KURT(X)
}
\arguments{
  \item{X}{random variable}
}
\description{
  Kurtosis of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
KURT(fair.die)
}

