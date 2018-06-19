##' Generalization function for the "replaced elements" model.
##'
##' rem() computes generalization based on the "replaced elements"
##' model.
##' @title rem
##' @param X Name of stimulus vector (as string) 
##' @param Y Name of stimulus vector (as string)
##' @return Generalization value
##' @author Stefano Ghirlanda
rem <- function( X, Y ) {
    X <- name2vec( X )
    Y <- name2vec( Y )

    nX <- sum(X)
    nY <- sum(Y)
    nXY <- sum(X*Y)

    r <- alsim.options$rem$r

    ## each cue comes with a 1-r inhibition factor:
    X <- (1-r)^(nX-1) * X
    Y <- (1-r)^(nY-1) * Y

    ## this determines the number of common pairs, which restores some
    ## of the inhibited elements. we construct pairs of elements with
    ## combn() and then use duplicated() to count the common pairs:
    if( nX>1 && nY>1 ) {
	pX <- t(combn(which(X>0),2))
	pY <- t(combn(which(Y>0),2))
	nReplacements <- sum(duplicated(rbind(pX,pY)))
    } else {
	nReplacements <- 0
    }

    sum( X * Y ) + r * nReplacements
}
