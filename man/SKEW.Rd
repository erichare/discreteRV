\name{SKEW}
\alias{SKEW}
\title{Skewness of a random variable}
\usage{
  SKEW(X)
}
\arguments{
  \item{X}{random variable}
}
\description{
  Skewness of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
SKEW(fair.die)
}

