\name{\%OR\%}
\alias{\%OR\%}
\title{Compute the logical OR of two events}
\usage{
X \%OR\% Y
}
\arguments{
  \item{X}{RVcond object}

  \item{Y}{RVcond object}
}
\value{
An RVresult object which is two events ORed together
}
\description{
Compute the logical OR of two events
}
\examples{
X.fair.die <- make.RV(1:6, rep("1/6",6))
P((X.fair.die == 4) \%OR\% (X.fair.die == 3))
}

