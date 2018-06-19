##' Look up a vector by name
##'
##' Given an object name as a string, name2vec returns the object
##' itself. It only looks for numeric objects in the parent
##' environment.
##' @title name2vec
##' @param X A numeric object's name
##' @return The numeric object itself
##' @author Stefano Ghirlanda 
##' @export
name2vec <- function( X ) {
    get( X, pos=1, mode="numeric" )
}
