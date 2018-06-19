checkDesign <- function( design ) {
}
##' Run an experiment in alsim
##'
##' An experiment is a list of groups, a group is a list of phases. For what a phase is see the documentation to phase. 
##' @title experiment
##' @param model An alsim model
##' @param design An experimental design
##' @param group.size Hown many subjects to run in each group
##' @return An updated alsim model object
##' @author Stefano Ghirlanda
##' @export
experiment <- function( model, design, group.size ) {
    checkDesign( design )

    group <- names( design )

    results <- list()

    for( g in names( design ) ) {
	results[[g]] <- rep( list(), group.size )
	for( i in 1:group.size ) {
	    results[[g]][[i]] <- model
	    n <- length( design[[g]] )
	    for( k in 1:n ) {
		results[[g]][[i]] <- phase(
		    results[[g]][[i]],
		    design[[g]][[k]]
		)
	    }
	}
    }
    results
}
