exploreOutcomes <- function(outcomes, probs) {
    final.outcomes <- NULL
    
    if (!is.finite(outcomes[1]) & !is.finite(outcomes[2])) {
        curr <- 0
        out <- NULL
        
        while (probs(curr) > 0) {
            out <- c(out, curr)
            curr <- curr + 1
        }
        
        curr <- -1
        
        while (probs(curr) > 0) {
            out <- c(out, curr)
            curr <- curr - 1
        }
        
        final.outcomes <- out
    } else if (!is.finite(outcomes[2])) {
        curr <- outcomes[1]
        out <- NULL
        
        while (probs(curr) > 0) {
            out <- c(out, curr)
            curr <- curr + 1
        }
        
        final.outcomes <- out
    } else if (!is.finite(outcomes[1])) {
        curr <- outcomes[2]
        out <- NULL
        
        while (probs(curr) > 0) {
            out <- c(out, curr)
            curr <- curr - 1
        }
        
        final.outcomes <- out
    } else {
        final.outcomes <- outcomes[1]:outcomes[2]
    }
    
    return(final.outcomes)
}

#' Make a random variable consisting of possible outcome values and their probabilities or odds
#' 
#' @name make.RV
#' @docType package
#' @param outcomes vector of possible outcomes
#' @param probs vector of probabilities or function defining probabilities
#' @param odds vector of odds
#' @param fractions If TRUE, return the probabilities as fractions when printing
#' @param range If TRUE, outcomes specify a range of values in the form c(lower, upper)
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
#' X.biased.coin <- make.RV(c(2,-1), odds = c(1,2))
#' 
#' # Make a fair die
#' X.fair.die <- make.RV(1:6, 1/6)
#' 
#' # Make a loaded die, specifying odds 1:1:1:1:2:4 rather than probabilities:
#' X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
#' 
#' # Make a Poisson random variable
#' pois.func <- function(x, lambda = 5) { lambda^x * exp(-lambda) / factorial(x) }
#' X.pois <- make.RV(c(0, Inf), pois.func)
make.RV <- function(outcomes, probs = NULL, odds = NULL, fractions = (class(probs) != "function"), range = any(is.infinite(outcomes))) {
    
    test <- fractions # TODO: Fix
    if (range) outcomes <- suppressWarnings(exploreOutcomes(outcomes, probs))
    if (class(probs) == "function") probs <- suppressWarnings(probs(outcomes))
    
    pr <- probs
    if (is.null(pr)) pr <- odds
    
    probsSum <- sum(pr)
    
    if (probsSum > 1 & is.null(odds)) stop("Probabilities sum to over 1")
    if (any(pr < 0)) stop("Probabilities cannot be negative")
    
    isOdds <- !is.null(odds)
    
    if (length(outcomes) < length(pr)) {
        stop("More probabilities/odds than outcomes provided")
    } else if (length(outcomes) > length(pr)) {
        pr <- c(pr, rep(ifelse(isOdds, 1, (1 - probsSum) / (length(outcomes) - length(pr))), length(outcomes) - length(pr)))
    }
    
    ## Convert to probs
    probs <- pr / sum(pr)
    names(probs) <- outcomes
    
    class(outcomes) <- "RV"
    
    attr(outcomes, "probs") <- probs
    attr(outcomes, "odds") <- isOdds
    attr(outcomes, "fractions") <- fractions
    attr(outcomes, "range") <- range

    return(outcomes)
}

unopset <- function(X, Xchar, cond, x) {
    X.notrv <- X
    class(X.notrv) <- NULL
    
    result <- eval(parse(text = paste("X.notrv", cond, "c(", paste(x, collapse = ","), ")")))
    class(result) <- "RVresult"
    
    attr(result, "outcomes") <- as.vector(X)
    attr(result, "rv") <- Xchar
    attr(result, "probs") <- probs(X)
    
    return(result)
}

binopset <- function(X, Xchar, cond, Y) {
    result <- eval(parse(text = paste("as.logical(X)", cond, "as.logical(Y)")))
    class(result) <- "RVresult"
    
    attr(result, "outcomes") <- attr(X, "outcomes")
    attr(result, "rv") <- Xchar
    attr(result, "probs") <- probs(X)
        
    return(result)
}

#' @export
"<.RV" <- function(X, x) { return(unopset(X, deparse(substitute(X)), "<", x)) }
#' @export
"<=.RV" <- function(X, x) { return(unopset(X, deparse(substitute(X)), "<=", x)) }
#' @export
"==.RV" <- function(X, x) { return(unopset(X, deparse(substitute(X)), "==", x)) }
#' @export
"!=.RV" <- function(X, x) { return(unopset(X, deparse(substitute(X)), "!=", x)) }
#' @export
">=.RV" <- function(X, x) { return(unopset(X, deparse(substitute(X)), ">=", x)) }
#' @export
">.RV" <- function(X, x) { return(unopset(X, deparse(substitute(X)), ">", x)) }


#' Generic method for in operator function
#' 
#' @name %in%
#' @param e1 First vector
#' @param e2 Second vector
#' @return A logical vector indicating which elements of e1 are in e2
#' @export
"%in%" <- function(e1, e2) { UseMethod("%in%") } 

#' @export
"%in%.default" <- function(e1, e2) { base::`%in%`(e1, e2) }
#' @export
"%in%.RV" <- function(e1, e2) { return(unopset(e1, deparse(substitute(e1)), "%in%", e2)) }

#' Compute the logical OR of two events
#' 
#' @name %OR%
#' @param X RVcond object
#' @param Y RVcond object
#' @return An RVresult object which is two events ORed together
#' @export
#' @examples
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
#' P((X.fair.die == 4) %OR% (X.fair.die == 3))
"%OR%" <- function(X, Y) { return(binopset(X, deparse(substitute(X)), "|", Y)) }

#' Compute the logical AND of two events
#' 
#' @name %AND%
#' @param X RVcond object
#' @param Y RVcond object
#' @return An RVresult object which is two events ANDed together
#' @export
#' @examples
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
#' P((X.fair.die == 4) %AND% (X.fair.die == 3))
"%AND%" <- function(X, Y) { return(binopset(X, deparse(substitute(X)), "&", Y)) }

#' Compute the conditional probability of two events
#' 
#' @name Conditional
#' @aliases |.RVresult
#' @param vec1 an RVresult object
#' @param vec2 an RVresult object
#' @return An RVcond object representing the conditional probability
#' @export
#' @examples
#' X.fair.die <- make.RV(1:6, rep(1/6, 6))
#' X.fair.coin <- make.RV(1:2, rep(1/2, 2))
#' 
#' P(X.fair.die == 4 | X.fair.die > 3)
#' P(X.fair.die == 5 | X.fair.die < 5)
#' P(X.fair.die == 4 | X.fair.coin == 1) # Independence
"|.RVresult" <- function(vec1, vec2) {
    cond1 <- (attr(vec1, "rv") == attr(vec2, "rv"))
    cond2 <- (gsub("[(=<>) 1-9]", "", paste(as.character(attr(vec1, "rv")), collapse = "")) == gsub("[(=<>) 1-9]", "", paste(as.character(attr(vec2, "rv")), collapse = "")))
    cond3 <- P(vec2) == 0
    
    result <- if (cond1 | cond2 | cond3) P(vec1 %AND% vec2) / P(vec2) else P(vec1)
    class(result) <- "RVcond"
    attr(result, "probs") <- attr(vec1, "probs")
    
    return(result)
}

#' @export
"print.RVcond" <- function(x, ...) {
    return(print.default(as.numeric(x)))
}

#' @export
"print.RVresult" <- function(x, ...) {
    vec <- (as.logical(x))
    names(vec) <- attr(x, "outcomes")
    
    return(print.default(vec, ...))
}

#' Outcomes of random variable X 
#'
#' Obtain the list of outcomes from a random variable
#'
#' @param X random variable
#' @return vector of outcomes of X
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' outcomes(X.Bern)
#' 
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
#' outcomes(X.fair.die)
#' 
#' X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
#' outcomes(X.loaded.die)
outcomes <- function(X) {
    return(as.vector(X))
}

#' Probability mass function of random variable X 
#'
#' Obtain the list of probabilities from a random variable: p(x)
#'
#' @param X random variable
#' @return named vector of probablities for each element of the random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' probs(X.Bern)
#' 
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
#' probs(X.fair.die)
#' 
#' X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
#' probs(X.loaded.die)
probs <- function(X) { 
    return(attr(X, "probs"))
}

#' Joint probability mass function of random variables X and Y
#'
#' @author Heike Hofmann \email{hofmann@@iastate.edu}
#' @param X random variable
#' @param Y random variable
#' @param sep separator between items from marginal distributions, by default set to ","
#' @param fractions If TRUE, return the probabilities as fractions
#' @export
#' @examples
#' d <- make.RV(c("A","B","C"), odds = c(3,5,11))
#' d2 <- mult(d,d)
#' probs(d2)
mult <- function(X, Y, sep=",", fractions = (attr(X, "fractions") & attr(Y, "fractions"))) {
    S <- X
    tmp <- tapply(outer(probs(S), probs(Y), FUN="*"),
                  outer(S, Y, FUN="paste", sep=sep), paste, sep=sep)
    S <- names(tmp)
    class(S) <- "RV"
    
    attr(S, "probs") <- as.numeric(tmp)
    attr(S, "fractions") <- fractions
    attr(S, "odds") <- FALSE
    attr(S, "range") <- attr(X, "range")

    return(S)
}

#' Probability mass function of  X^n
#'
#' @author Heike Hofmann \email{hofmann@@iastate.edu}
#' @param X random variable
#' @param n power
#' @param sep separator between items from marginal distributions, by default set to ","
#' @param fractions If TRUE, return the probabilities as fractions
#' @export
#' @examples
#' d <- make.RV(c("A","B","C"), odds = c(3,5,11))
#' d2 <- multN(d)
#' probs(d2)
multN <- function(X, n=2, sep=",", fractions=attr(X, "fractions")) {
    S <- X;  i <- 2
    while(i<=n) {
        tmp <- tapply(outer(probs(S), probs(X), FUN="*"),
                      outer(S, X, FUN="paste", sep=sep), paste, sep=sep)
        S <- names(tmp)
        attr(S, "probs") <- as.numeric(tmp)
        i <- i+1
    }
    class(S) <- "RV"
    
    attr(S, "fractions") <- fractions
    attr(S, "odds") <- FALSE
    attr(S, "range") <- attr(X, "range")

    return(S)
}

#' Turn a probability vector with possible outcome values in the 'names()' attribute
#' into a random variable:
#'
#' @param px A probability vector with possible outcome values in the 'names()' attribute
#' @param fractions If TRUE, return the probabilities as fractions
#' 
#' @export
as.RV <- function(px, fractions = TRUE) {
    X <- as.numeric(names(px))
    
    class(X) <- "RV"
    
    attr(X, "probs") <- px
    attr(X, "fractions") <- fractions
    attr(X, "odds") <- FALSE
    attr(X, "range") <- FALSE
    
    X
}

#' Calculate probabilities of events
#'
#' @param event A logical vector
#' @export
#' @examples
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
#' P(X.fair.die>3)
#' 
#' X.loaded.die <- make.RV(1:6, odds = c(1,1,1,1,2,4))
#' P(X.loaded.die>3)
#' P(X.loaded.die==6)
P <- function(event) { UseMethod("P") } 

#' @export
P.default <- function(event) { sum(attr(event, "probs")[event]) }

#' @export
P.RVcond <- function(event) { return(event) }

#' Expected value of a random variable
#' 
#' @param X random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' E(X.Bern)
#' 
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
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
#' @param fractions If TRUE, return the probabilities as fractions
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' X.fair.die <- make.RV(1:6, rep(1/6,6))
#' 
#' S5 <- SofI(X.Bern, X.Bern, X.Bern, X.Bern, X.Bern)  
#' S.mix <- SofI(X.Bern, X.fair.die)  # Independent but not IID
SofI <- function(..., fractions=attr(list(...)[[1]], "fractions")) {
    LIST <- list(...)
    S <- LIST[[1]]
    LIST <- LIST[-1]
    while(length(LIST)>0) {
        X <- LIST[[1]]
        tmp <- tapply(outer(probs(S), probs(X), FUN="*"),
                      outer(S,        X,        FUN="+"), sum)
        S <- as.numeric(names(tmp))  
        attr(S, "probs") <- tmp
        LIST <- LIST[-1]
    }
    class(S) <- "RV"
    
    attr(S, "fractions") <- fractions
    attr(S, "odds") <- FALSE
    attr(S, "range") <- attr(list(...)[[1]], "range")
    
    return(S)
}

#' Sum of independent identically distributed random variables
#' 
#' @param X A random variable
#' @param n The number of Xs to sum
#' @param fractions If TRUE, return the probabilities as fractions
#' @param progress If TRUE, display a progress bar
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' 
#' S5 <- SofIID(X.Bern, 5)
#' S128 <- SofIID(X.Bern, 128)
SofIID <- function(X, n=2, progress=TRUE, fractions=attr(X, "fractions")) {
    S <- X;  i <- 2
    pb <- txtProgressBar(min = 1, max = n)
    while(i<=n) {
        tmp <- tapply(outer(probs(S), probs(X), FUN="*"),
                      outer(S,        X,        FUN="+"), sum)
        
        S <- as.numeric(names(tmp))  
        attr(S, "probs") <- tmp
        
        if(i%%100==0 & progress) setTxtProgressBar(pb, i)
        i <- i+1
    };
    close(pb)
    
    class(S) <- "RV"
    
    attr(S, "fractions") <- fractions
    attr(S, "odds") <- FALSE
    attr(S, "range") <- attr(X, "range")
    
    return(S)
}

#' Plot a random variable of class "RV"
#' 
#' @method plot RV
#' @param x A random variable
#' @param ... Additional arguments to be passed to the "plot" function
#' @param tol Only display outcomes with probabilities above tol
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
#' @param lwd The line width, a positive number, defaulting to 2.
#' @param col A specification for the default plotting color
#' @param xlab Label for the X axis
#' @param ylab Label for the Y axis
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6,6))
#' plot(fair.die)
plot.RV <- function(x, ..., tol=1e-10, pch=16, cex=1.2, lwd=2, col="black",
                    xlab="Possible Values",
                    ylab="Probabilities") {
    ## args <- list(...);  print(args)
    xx <- x[probs(x) > tol]
    px <- probs(x)[probs(x) > tol]
    plot(xx, px, type="h", lwd=lwd, col=col, xlab=xlab, ylab=ylab, ...)
    abline(h=0, col="gray")
    points(xx, px, pch=pch, cex=cex, col=col)
}

#' Print a random variable of class "RV"
#' 
#' @method print RV
#' @author Eric Hare \email{erichare@@iastate.edu}
#' @param x A random variable
#' @param odds If TRUE, print as odds instead of probs
#' @param fractions If TRUE, print probs as fractions instead of decimals
#' @param all.outcomes If TRUE, print all outcomes rather than the first ten
#' @param digits Number of digits to print for probabilities
#' @param ... Additional arguments to be passed to the "format" function
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6,6))
#' print(fair.die)
print.RV <- function(x, odds = attr(x, "odds"), fractions = attr(x, "fractions"), all.outcomes = FALSE, digits = 3, ...) {
    attributes(x)$class <- NULL
        
    vec <- attr(x, "probs")
    if (!fractions) vec <- round(vec, digits = digits)
    
    if (odds) vec <- vec / min(vec[vec > 0])
    if (fractions) {require(MASS); vec <- fractions(vec)}
    names(vec) <- x
    
    if (odds) type <- "Odds" else type <- "Probs"
    if (fractions & !odds) vec <- as.character(vec)
    if (odds) vec <- paste(round(vec, digits = digits), round(sum(as.numeric(vec)) - as.numeric(vec), digits = digits), sep = ":")
    
    df <- eval(parse(text = paste("data.frame(Outcomes = as.character(x), ", type, " = vec)", sep = "")))
    names(df)[2] <- paste(type, paste(rep(" ", nchar("Outcomes") - nchar(type) - 1), collapse = ""))
    
    old.df <- df
    
    cat(paste("Random variable with", length(x), "outcomes\n\n"))
    
    if (nrow(df) > 12 & !all.outcomes) df <- df[1:12,]
    write.table(format(t(df), justify = "right", ...), col.names = FALSE, quote = FALSE)

    if (nrow(old.df) > 12) cat("\nDisplaying first 12 outcomes\n")
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
#' @param tol tolerance for the zero probability case
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6,6))
#' qqnorm(fair.die)
qqnorm.RV <- function(y, ..., pch=16, cex=.5, add=FALSE, xlab="Normal Quantiles", ylab="Random Variable Quantiles", tol = 1e-10) {
    ind <- which(probs(y) > tol)
    outcomes <- sort(y[ind])
    pc <- cumsum(probs(y))[ind]
    
    if (!add) {
        plot(qnorm(pc), outcomes, pch=pch, cex=cex, xlab=xlab, ylab=ylab, ...)
    } else {
        points(qnorm(pc), outcomes, pch=pch, cex=cex, ...)
    }
}

#' Marginal distribution of a joint random variable
#'
#' Extracts the marginal probability mass functions from a joint distribution.
#' @author Heike Hofmann \email{hofmann@@iastate.edu}
#' @param X a random variable
#' @param sep parameter specifying the separator between dimensions, defaults to ","
#' @export
#' @examples
#' X <- make.RV(1:6, 1/6)
#' X3 <- multN(X, 3)
#' margins(X3)
margins <- function(X, sep=",") {
    dframe <- sapply(strsplit(as.character(X), split=sep, fixed=TRUE), function(x) as.matrix(x))
    
    require(plyr)
    res <- alply(dframe, .margins=1, function(x) {
        dtab <- xtabs(probs(X)~x)
        make.RV(names(dtab), as.numeric(dtab))
    })    
    res
}
