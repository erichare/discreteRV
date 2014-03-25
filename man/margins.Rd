\name{margins}
\alias{margins}
\title{Marginal distribution of a joint random variable}
\usage{
margins(X, sep = ",")
}
\arguments{
  \item{X}{a random variable}

  \item{sep}{parameter specifying the separator between
  dimensions, defaults to ","}
}
\description{
Extracts the marginal probability mass functions from a
joint distribution.
}
\examples{
X <- make.RV(1:6, odds = 1:6)
X3 <- multN(X, 3)
margins(X3)
}
\author{
Heike Hofmann \email{hofmann@iastate.edu}
}

