\name{E}
\alias{E}
\title{Expected value of a random variable}
\usage{
  E(X)
}
\arguments{
  \item{X}{random variable}
}
\description{
  Expected value of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
E(fair.die)
}

