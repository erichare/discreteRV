\name{Conditional}
\alias{Conditional}
\alias{|.RVresult}
\title{Compute the conditional probability of two events}
\usage{
\method{|}{RVresult}(vec1, vec2)
}
\arguments{
  \item{vec1}{an RVresult object}

  \item{vec2}{an RVresult object}
}
\value{
An RVcond object representing the conditional probability
}
\description{
Compute the conditional probability of two events
}
\examples{
X.fair.die <- make.RV(1:6, rep("1/6", 6))
X.fair.coin <- make.RV(1:2, rep("1/2", 2))

P(X.fair.die == 4 | X.fair.die > 3)
P(X.fair.die == 5 | X.fair.die < 5)
P(X.fair.die == 4 | X.fair.coin == 1) # Independence
}

