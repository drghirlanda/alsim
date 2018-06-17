##' Generalization function for Pearce's "configural" model.
##'
##' rem() computes generalization according to Pearce's (1987,1994)
##' "configural" model of associative learning.  model.
##' @title pearce
##' @param X Name of stimulus vector (as string)
##' @param Y Name of stimulus vector (as string)
##' @return Generalization value
##' @author Stefano Ghirlanda
pearce <- function( X, Y ) {
    X <- name2vec( X )
    Y <- name2vec( Y )
    sum( X*Y )^2 / (sum( X*X ) * sum( Y*Y ))
}
