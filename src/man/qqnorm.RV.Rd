\name{qqnorm.RV}
\alias{qqnorm.RV}
\title{Normal quantile plot for RVs to answer the question how close to normal it is}
\usage{
  \method{qqnorm}{RV} (y, ..., pch = 16, cex = 0.5,
    add = FALSE, xlab = "Normal Quantiles",
    ylab = "Random Variable Quantiles")
}
\arguments{
  \item{y}{A random variable}

  \item{...}{Additional arguments to be passed to the
  "plot" or "points" function}

  \item{pch}{Either an integer specifying a symbol or a
  single character to be used as the default in plotting
  points.}

  \item{cex}{A numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default.}

  \item{add}{A logical indicating whether to add to an
  existing plot}

  \item{xlab}{Label for the X axis}

  \item{ylab}{Label for the Y axis}
}
\description{
  Normal quantile plot for RVs to answer the question how
  close to normal it is
}
\examples{
fair.die <- make.RV(1:6, rep("1/6",6))
qqnorm(fair.die)
}

