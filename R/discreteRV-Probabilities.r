#' Make a random variable consisting of possible outcome values and their probabilities or odds
#' 
#' @name make.RV
#' @docType package
#' @param vals vector of possible outcomes
#' @param probs.or.odds vector of probabilities or odds.
#' @return random variable as RV object.
#' @export
#' @examples
#' # Make a 50:50 Bernoulli random variable:
#' X.Bern <- make.RV(c(1,0), c(.5,.5))   
#'   
#' # Make a fair coin flip game with payoffs +$1 and -$1:
#' X.fair.coin <- make.RV(c(1,-1), c(.5,.5))
#' 
#' # Make a biased coin flip game with odds 1:2 and with fair payoffs +$2 and -$1
#' X.biased.coin <- make.RV(c(2,-1), c(1,2))
#' 
#' # Make a fair die
#' X.fair.die <- make.RV(1:6, rep("1/6",6))
#' 
#' # Make a loaded die, specifying odds 1:1:1:1:2:4 rather than probabilities:
#' X.loaded.die <- make.RV(1:6, c(1,1,1,1,2,4))
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
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' probs(X.Bern)
#' 
#' X.fair.die <- make.RV(1:6, rep("1/6",6))
#' probs(X.fair.die)
#' 
#' X.loaded.die <- make.RV(1:6, c(1,1,1,1,2,4))
#' probs(X.loaded.die)
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

#' Turn a probability vector with possible outcome values in the 'names()' attribute
#' into a random variable:
#'
#' @param px A probability vector with possible outcome values in the 'names()' attribute
#' @export
as.RV <- function(px) {
    X <- as.numeric(names(px))
    names(X) <- px
    class(X) <- "RV"
    X
}

#' Calculate probabilities of events
#'
#' @param event A logical vector, with names equal to the probabilities
#' @export
#' @examples
#' X.fair.die <- make.RV(1:6, rep("1/6",6))
#' P(X.fair.die>3)
#' 
#' X.loaded.die <- make.RV(1:6, c(1,1,1,1,2,4))
#' P(X.loaded.die>3)
#' P(X.loaded.die==6)
P <- function(event) { sum(probs(event)[event]) }

#' Expected value of a random variable
#' 
#' @param X random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' E(X.Bern)
#' 
#' X.fair.die <- make.RV(1:6, rep("1/6",6))
#' E(X.fair.die)
E <- function(X) { sum(X*probs(X)) }

#' Variance of a random variable
#' 
#' @param X random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' E(X.Bern)
V <- function(X) { E((X-E(X))^2) }

#' Standard deviation of a random variable
#' 
#' @param X random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' E(X.Bern)
SD <- function(X) { sqrt(V(X)) }

#' Skewness of a random variable
#' 
#' @param X random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' SKEW(X.Bern)
SKEW <- function(X) { E((X-E(X))^3)/SD(X)^3 }

#' Kurtosis of a random variable
#'
#' @param X random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' KURT(X.Bern)
KURT <- function(X) { E((X-E(X))^4)/V(X)^2 }

#' Sum of independent random variables
#' 
#' @param ... Arbitrary number of random variables
#' @param digits number of digits of precision used in the calculation. By defualt set to 15. 
#' @param scipen A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' X.fair.die <- make.RV(1:6, rep("1/6",6))
#' 
#' S5 <- SofI(X.Bern, X.Bern, X.Bern, X.Bern, X.Bern)  
#' S.mix <- SofI(X.Bern, X.fair.die)  # Independent but not IID
SofI <- function(..., digits=15, scipen=10) {
    LIST <- list(...)
    S <- LIST[[1]]
    LIST <- LIST[-1]
    while(length(LIST)>0) {
        X <- LIST[[1]]
        tmp <- tapply(outer(probs(S), probs(X), FUN="*"),
                      outer(S,        X,        FUN="+"), sum)
        S <- as.numeric(names(tmp))  
        options(digits=digits, scipen=scipen) 
        names(S) <- format(tmp)
        LIST <- LIST[-1]
    }
    class(S) <- "RV"
    return(S)
}

#' Sum of independent identically distributed random variables
#' 
#' @param X A random variable
#' @param n The number of Xs to sum
#' @param digits number of digits of precision used in the calculation. By defualt set to 15. 
#' @param scipen A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' 
#' S5 <- SofIID(X.Bern, 5)
#' S128 <- SofIID(X.Bern, 128)
SofIID <- function(X, n=2, digits=15, scipen=10) {
    S <- X;  i <- 2
    while(i<=n) {
        tmp <- tapply(outer(probs(S), probs(X), FUN="*"),
                      outer(S,        X,        FUN="+"), sum)
        S <- as.numeric(names(tmp))  
        options(digits=digits, scipen=scipen) 
        names(S) <- format(tmp)
        if(i%%100==0) cat(i,"... ")
        i <- i+1
    };   cat("\n")
    class(S) <- "RV"
    return(S)
}

#' Plot a random variable of class "RV"
#' 
#' @method plot RV
#' @param x A random variable
#' @param ... Additional arguments to be passed to the "plot" function
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
#' @param lwd The line width, a positive number, defaulting to 2.
#' @param col A specification for the default plotting color
#' @param stretch.x A numeric by which to extend the x axis limits
#' @param stretch.y A numeric by which to extend the y axis limits
#' @param xlab Label for the X axis
#' @param ylab Label for the Y axis
#' @param xlim Lower and upper limit for the x axis
#' @param ylim Lower and upper limit for the y axis
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep("1/6",6))
#' plot(fair.die)
plot.RV <- function(x, ..., pch=16, cex=1.2, lwd=2, col="black",
                    stretch.x=1.2, stretch.y=1.2,
                    xlab="Possible Values",
                    ylab="Probabilities",
                    xlim=mean(range(x)) + (range(x)-mean(range(x)))*stretch.x,
                    ylim=c(0, max(probs(x))*stretch.y)) {
    ## args <- list(...);  print(args)
    xx <- as.numeric(x)
    px <- probs(x)
    plot(xx, px, type="h", lwd=lwd, col=col, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, ...)
    abline(h=0, col="gray")
    points(xx, px, pch=pch, cex=cex, col=col)
}

#' Print a random variable of class "RV"
#' 
#' @method print RV
#' @param x A random variable
#' @param ... Additional arguments to be passed to the "print" function
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep("1/6",6))
#' print(fair.die)
print.RV <- function(x, ...) {
    attributes(x)$class <- NULL
    print(x, ...)
}

#' Normal quantile plot for RVs to answer the question how close to normal it is
#'
#' @method qqnorm RV
#' @param y A random variable
#' @param ... Additional arguments to be passed to the "plot" or "points" function
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
#' @param add A logical indicating whether to add to an existing plot
#' @param xlab Label for the X axis
#' @param ylab Label for the Y axis
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep("1/6",6))
#' qqnorm(fair.die)
qqnorm.RV <- function(y, ..., pch=16, cex=.5, add=FALSE, 
                      xlab="Normal Quantiles", ylab="Random Variable Quantiles") {
    y <- sort(y[probs(y)>0])
    pc <- cumsum(probs(y))
    if(!add) {
        plot(qnorm(pc), y, pch=pch, cex=cex, xlab=xlab, ylab=ylab, ...)
    } else {
        points(qnorm(pc), y, pch=pch, cex=cex, ...)
    }
}
