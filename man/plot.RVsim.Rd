\name{plot.RVsim}
\alias{plot.RVsim}
\title{Plot a simulated random vector}
\usage{
\method{plot}{RVsim}(x, ...)
}
\arguments{
  \item{x}{A simulated data vector produced with the
  'rsim()' function}

  \item{...}{Additional arguments to be passed to the
  'plot()' function}
}
\description{
Plot a simulated random vector
}
\examples{
X <- make.RV(c(100000,10000,0), c(0.00025,0.005,0.99475))
X.sim <- rsim(200000, X)

plot(X.sim)
}

