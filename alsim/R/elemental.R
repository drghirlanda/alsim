##' Generalization function for elemental models.
##'
##' elemental() computes generalization according the inner product
##' rule appropriate for elemental models of associative learning.
##' @title elemental
##' @param X Name of stimulus vector (as string)
##' @param Y Name of stimulus vector (as string)
##' @return Generalization value
##' @author Stefano Ghirlanda
##' @export
elemental <- function( X, Y ) {
    X <- name2vec( X )
    Y <- name2vec( Y )
    sum( X*Y )
}
