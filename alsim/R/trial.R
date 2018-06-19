##' Perform a single learning trial
##'
##' Performs a single learning trial on a model, given a stimulus X
##' and desired associative strength lambda.
##' @title trial
##' @param model An alsim model
##' @param X A stimulus name (as string)
##' @param lambda Desired associative strength
##' @return Updated model
##' @author Stefano Ghirlanda
##' @export
trial <- function( model, X, lambda ) {
    n <- length( model$trials )

    if( ! X %in% names(model$weights) ) {
	model$weights[[ X ]] <- rep(0,n)
    }

    for( Y in names(model$weights) ) {
	if( Y == X ) {
	    v <- as.numeric( V( model, X )[1,] )
	    d <- model$rate * ( lambda - v )
	} else {
	    d <- 0
	}
	if( n ) {
	    wOld <- model$weights[[ Y ]][ n ]
	} else {
	    wOld <- 0
	}
	wNew <-  wOld + d
	model$weights[[ Y ]] <- c( model$weights[[ Y ]], wNew )
    }

    model$trials <- c( model$trials, X )
    model$lambda <- c( model$lambda, lambda )

    model
}
