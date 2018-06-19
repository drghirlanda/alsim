##' Modify stimulus vectors according to the "replaced elements" model.
##'
##' remify(X) modifies stimulus vector X according to the
##' "replaced elements" model, so that the elemental() generalization
##' function can be used on remify'ed vectors to simulate the replaced
##' elements model.
##' @title remify
##' @param X Name of stimulus vector (as string)
##' @return Generalization value
##' @author Stefano Ghirlanda
##' @export
remify <- function( X ) {
    X <- name2vec( X )
    nX <- sum(X)
    r <- alsim.options$rem$r

    ## inhibition factor for a stimulus with nX elements:
    f <- (1-r)^(nX-1)

    X <- f * X

    ## this is the number of potential element pairs:
    pX <- combn( length(X), 2 )

    ## vector as long as there are pairs
    P <- rep( 0, ncol(pX) )

    ## set each pair indicator to 2*r if the pair is set in X:
    activePairs <- 0
    for( i in 1:length(P) ) {
	if( sum( X[ pX[,i] ] > 0 ) == 2 ) {
	    P[i] <- nX*( 1 - f^2 )
	    activePairs <- activePairs + 1
	}
    }
    if( activePairs==0 )
	activePairs <- 1
    P <- sqrt( P / activePairs )

    c(X, P)
}
