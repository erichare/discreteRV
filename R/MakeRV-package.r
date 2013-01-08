#' MakeRV
#' 
#' Make a random variable consisting of possible outcome values and their probabilities or odds
#' 
#' @name make.RV
#' @docType package
#' @param vals vector of possible outcomes
#' @param probs.or.odds vector of probabilities or odds.
#' @return random variable as RV object.
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep("1/6",6))
make.RV <- function(vals, probs.or.odds) { 
  names(vals) <- probs.or.odds
  class(vals) <- "RV"
  vals 
} 



#' Probability mass function of random variable X 
#'
#' Obtain the list of probabilities from a random variable: p(x)
#'
#' @param X random variable
#' @param digits number of digits of precision used in the calculation. By defualt set to 15. 
#' @param scipen A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider
#' @return named vector of probablities for each element of the random variable
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep("1/6",6))
#' probs(fair.die)
probs <- function(X, scipen=10, digits=22) { 
  pr <- sapply(names(X), function(pstr) eval(parse(text=pstr)));
  options(scipen=scipen)
  names(pr) <- X
  pr <- pmax(pr,0)
  if(any(is.na(pr))) pr[is.na(pr)] <- pmax(0, (1-sum(pr[!is.na(pr)]))/sum(is.na(pr)))
  pr/sum(pr) 
}


#' Joint probability mass function of random variables X and Y
#'
#' @param X random variable
#' @param Y random variable
#' @param digits number of digits of precision used in the calculation. By defualt set to 15. 
#' @param scipen A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider
#' @export
#' @examples
#' d <- make.RV(c("A","B","C"), c(3,5,11))
#' d2 <- mult(d,d)
#' probs(d2)
mult <- function(X, Y, digits=15, scipen=10) {
  S <- X
  tmp <- tapply(outer(probs(S), probs(Y), FUN="*"),
                outer(S, Y, FUN="paste", sep=""), paste, sep="")
  S <- as.character(names(tmp))
  names(S) <- as.numeric(tmp)
  class(S) <- "RV"
  return(S)
}

#' Probability mass function of  X^n
#'
#' @param X random variable
#' @param n power
#' @param digits number of digits of precision used in the calculation. By defualt set to 15. 
#' @param scipen A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider
#' @export
#' @examples
#' d <- make.RV(c("A","B","C"), c(3,5,11))
#' d2 <- multN(d)
#' probs(d2)
multN <- function(X, n=2, digits=30, scipen=20) {
  S <- X;  i <- 2
  while(i<=n) {
    tmp <- tapply(outer(probs(S), probs(X), FUN="*"),
                  outer(S, X, FUN="paste", sep=""), paste, sep="")
    S <- as.character(names(tmp))
    names(S) <- as.numeric(tmp)
    i <- i+1
  }
  class(S) <- "RV"
  return(S)
}

