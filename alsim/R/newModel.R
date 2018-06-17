##' Create a new alsim model
##'
##' newModel creates a model given a generalization function and a learning rate
##' @title newModel
##' @param genFunction An alsim generalization function
##' @param learnRate  A learning rate
##' @return An alsim model
##' @author Stefano Ghirlanda
newModel <- function( genFunction, learnRate ) {
    m <- list(
	weights=list(),
	gen=genFunction,
	rate=learnRate,
	trials=c(),
	lambda=c(),
	V0=list()
    )
    class(m) <- "model"
    m
}
