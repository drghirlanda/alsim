##' Plot associative strengths for an alsim model
##'
##' MISSING
##' @title Vplot
##' @param model An alsim model
##' @param stimuli Stimuli whose associative strength should be
##'     plotted.
##' @param trials Vector of trials for which associative strengths
##'     should be plotted.
##' @param jitter Add vertical jitter. Sometimes useful to avoid
##'     overplotting
##' @return
##' @author
##' @export
Vplot <- function( model, stimuli, trials, jitter=0 ) {
    Vavg <- list()
    Vstd <- list()
    for( X in stimuli ) {
	v <- V( model, X, trials )
	Vavg[[X]] <- v[1,]
	Vstd[[X]] <- v[2,]
    }
    Tmax <- max(trials)
    Tmin <- min(trials)
    Vmax <- max( 1, max( unlist( lapply( Vavg, max ) ) ) )
    Vmin <- min( 0, min( unlist( lapply( Vavg, min ) ) ) )
    Vmax <- ( 1 + round(4*Vmax) )/4
    Vmin <- ( round(4*Vmin) - 1 )/4
    oldPar <- par( mar=c(4,4,2,5), las=1 )
    plot(
	c(Tmin,Tmax),
	c(Vmin, Vmax),
	pch=NA,
	xlab="Trial",
	ylab="V",
	xaxs="i",
	yaxs="i",
	yaxt="n"
    )
    axis( 2, seq(Vmin,Vmax,.5) )
    abline( h=seq(Vmin,Vmax,.25), lty=3 )
    nStim <- length(stimuli)
    for( i in 1:nStim ) {
	j <- runif( length(trials), -jitter, +jitter )
	y <- Vavg[[ stimuli[i] ]] + j
	z <- Vstd[[ stimuli[i] ]]
	lines( 
	    trials,
	    y,
	    col=i,
	    lwd=2,
	    xpd=TRUE
	)
	polygon(
	    c(trials,rev(trials)),
	    c(y-z,rev(y+z)),
	    col=adjustcolor(i,alpha=.2),
	    border=NA
	)
    }
    legend(
	Tmax, Vmax,
	legend=stimuli,
	col=1:nStim, lty=1, bty="n",
	xpd=TRUE, lwd=2
    )
    if( Vmin<0 ) abline( h=0, lty=3 )
    par( oldPar )
}
