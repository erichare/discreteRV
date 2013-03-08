#' Simulate n independent trials from a random variable X:
#' 
#' @param n The number of independent trials to simulate
#' @param X A random variable
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' X.Bern.sim100 <- rsim(100, X.Bern)
#' 
#' X.loaded.die <- make.RV(1:6, c(1,1,1,1,2,4))
#' X.loaded.die.sim100 <- rsim(100, X.loaded.die)
#' 
#' # The function 'rsim()' attaches the probabilities as names to the random draws.
#' # To get the values only, use 'as.vector()':
#' as.vector(X.Bern.sim100)
#' as.vector(X.loaded.die.sim100)
rsim <- function(n, X) { tmp <- sample(X, size=n, replace=T, prob=probs(X))
                         attributes(tmp)$RV <- X;  class(tmp) <- "RVsim";  tmp } 

#' Proportions of observed outcomes in one or more vectors of simulated trials
#' 
#' @param ... Simulation data produced with the 'rsim()' function
#' @export
#' @examples
#' X.Bern <- make.RV(c(1,0), c(.5,.5))
#' X.Bern.sim100 <- rsim(100, X.Bern)
#' 
#' X.loaded.die <- make.RV(1:6, c(1,1,1,1,2,4))
#' X.loaded.die.sim100 <- rsim(100, X.loaded.die)
#' props(X.Bern.sim100)
#' props(X.loaded.die.sim100)
#' # Note: 'props()' is the analog of 'probs()', but
#' #       'props()' applies to SIMULATION DATA and tabulates them, whereas
#' #       'probs()' applies to RANDOM VARIABLES and lists their probabilities.
#' #       By the LLN the results of 'props()' will be close to 'probs()' for
#' #       for large simulations.
props <- function(...) { LIST <- list(...)
                         LIST <- lapply(LIST, function(x) {
                             RV <- attributes(x)$RV
                             if(!is.null(RV)) { factor(x, levels=as.character(RV))
                             } else { x } } )
                         tbl <- table(RV=LIST)
                         tbl <- tbl/sum(tbl)
                         tbl
}

# Proportion of an event observed in a vector of simulated trials:
Prop <- function(X.sim) { sum(X.sim)/length(X.sim) }  

# Otherwise use the empirical functions
# mean(X.sim); var(X.sim); sd(X.sim); and the empirical skewness:
skew <- function(X.sim) { mean(scale(X.sim)^3) }

# Plot simulations like a random variable:
plot.RVsim <- function(Xsim, ...) {
    X <- as.RV(props(Xsim))
    plot(X, ylab="Proportions", ...)
}