\name{print.RV}
\alias{print.RV}
\title{Print a random variable of class "RV"}
\usage{
  \method{print}{RV} (x, ...)
}
\arguments{
  \item{x}{A random variable}

  \item{...}{Additional arguments to be passed to the
  "print" function}
}
\description{
  Print a random variable of class "RV"
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
print(fair.die)
}

