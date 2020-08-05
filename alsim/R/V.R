.V <- function( model, X, trial=NULL ) {
    if( class(model) != "model" )
	stop( "1st argument is not a model" )

    g <- list()
    v0 <- 0
    for( Y in names(model$weights) ) {
	g[[Y]] <- model$gen( Y, X )
	if( Y %in% names(model$V0) ) {
	    v0 <- v0 + g[[Y]] * model$V0[[ Y ]] 
	}
    }

    n <- length( model$trials )
    if( n==0 )
	return(v0)

    v <- rep(v0,n)
    for( Y in names(model$weights) ) {
	v <- v + g[[Y]] * model$weights[[ Y ]]
    }

    if( is.null(trial) ) # last trial
	trial <- n

    if( trial[1]==0 ) {
	v <- c( v0, v )
	trial <- trial + 1
    }

    v[ trial ]
}
##' Calculate associative strength
##'
##' V calculates the associative strengths of stimuli for a given
##' model
##' @title V
##' @param model An alsim model or list of models
##' @param X A stimulus name (string)
##' @param trial Trial or trials for which associative strength should
##'   be calculated. If NULL, use the last available trial.
##' @param aggr If TRUE, average results for all models given in input
##'   and calculate standard deviation. If FALSE, return results for
##'   all models.
##' @return If aggr==TRUE, a 2-dim matrix with rows corresponding to
##'   average and standard deviation, and columns corresponding to
##'   trials. If aggr==FALSE, a matrix with one row per model, and
##'   columns corresponding to trials.
##' @author Stefano Ghirlanda
##' @export
V <- function( model, X, trial=NULL, aggr=TRUE ) {
    if( class(model)=="model" ) {
	avg <- .V( model, X, trial )
	outputs <- matrix( rbind( avg, rep(NA,length(avg)) ), nrow=2 )
	rownames(outputs) <- c("avg","std")
	return( outputs )
    }
    outputs <- NULL
    for( i in 1:length(model) ) {
	outputs <- rbind(
	    outputs,
	    .V( model[[i]], X, trial )
	)
    }
    if( aggr==TRUE ) {
	avg <- apply( outputs, 2, mean )
	std <- apply( outputs, 2, sd )
	outputs <- rbind( avg, std )
	rownames(outputs) <- c("avg","std")
    } else {
	outputs <- matrix( outputs, nrow=length(model), byrow=TRUE )
	rownames(outputs) <- 1:length(model)
	colnames(outputs) <- trial
    }
    outputs
}
