\name{V}
\alias{V}
\title{Variance of a random variable}
\usage{
  V(X)
}
\arguments{
  \item{X}{random variable}
}
\description{
  Variance of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
V(fair.die)
}

