phase <- function( model, trials ) {
    stimuli <- names(trials)
    nTrials <- sum(unlist(lapply( trials, function(x){x[1]} )))
    nStim <- length(stimuli)
    sTrials <- NULL
    vLambda <- NULL
    for( s in stimuli ) {
	n <- trials[[s]][1]
	sTrials <- c( sTrials, rep(s, n) )
	vLambda <- c( vLambda, rep(trials[[s]][2], n) )
    }
    shuffled <- sample( 1:nTrials )
    for( t in shuffled ) {
	model <- trial( model, sTrials[t], vLambda[t] )
    }
    model
}
