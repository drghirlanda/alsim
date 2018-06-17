common <- function( X, active, commonFraction ) {
    n <- length(X)
    Y <- rep( 0, n )
    activeX <- which(X==1)
    nActiveX <- length(activeX)
    common <- round(n*commonFraction)
    nActiveCommon <- min(common,nActiveX)
    Y[ activeX[ 1:nActiveCommon ] ] <- 1
    notCommon <- setdiff( 1:n, activeX )
    Y[ notCommon[ 1:(active-nActiveCommon) ] ] <- 1
    Y
}
