\docType{package}
\name{make.RV}
\alias{make.RV}
\alias{make.RV-package}
\title{Make a random variable consisting of possible outcome values and their probabilities or odds}
\usage{
make.RV(outcomes, probs = NULL, odds = NULL, fractions = FALSE,
  range = FALSE, tol = 1e-10)
}
\arguments{
  \item{outcomes}{vector of possible outcomes}

  \item{probs}{vector of probabilities or function defining
  probabilities}

  \item{odds}{vector of odds}

  \item{fractions}{If TRUE, return the probabilities as
  fractions}

  \item{range}{If TRUE, outcomes specify a range of values
  in the form c(lower, upper)}

  \item{tol}{Any value below this level will be treated as
  zero probability}
}
\value{
random variable as RV object.
}
\description{
Make a random variable consisting of possible outcome
values and their probabilities or odds
}
\examples{
# Make a 50:50 Bernoulli random variable:
X.Bern <- make.RV(c(1,0), c(.5,.5))

# Make a fair coin flip game with payoffs +$1 and -$1:
X.fair.coin <- make.RV(c(1,-1), c(.5,.5))

# Make a biased coin flip game with odds 1:2 and with fair payoffs +$2 and -$1
X.biased.coin <- make.RV(c(2,-1), odds = c(1,2))

# Make a fair die
X.fair.die <- make.RV(1:6, rep("1/6",6))

# Make a loaded die, specifying odds 1:1:1:1:2:4 rather than probabilities:
X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
}

