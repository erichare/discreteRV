\name{SD}
\alias{SD}
\title{Standard deviation of a random variable}
\usage{
  SD(X)
}
\arguments{
  \item{X}{random variable}
}
\description{
  Standard deviation of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
SD(fair.die)
}

