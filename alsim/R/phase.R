##' Run multiple trials with multiple stimuli
##'
##' phase runs multiple learning trials with multiple stimuli. The
##' trials argument is a list whose names are stimulus names, with
##' each element a numeric vector giving the amount of trials and the
##' reinforcement. Thus list( A=c(50,1) ) runs a phase with 50
##' presentations of A and reinforcement of 1. When multiple stimuli
##' are used, the sequence of trials is randomized.
##' @title phase
##' @param model An alsim model object
##' @param trials A list of trial specifications (see details)
##' @return An updated alsim model object
##' @author Stefano Ghirlanda
##' @export
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
