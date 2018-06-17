checkDesign <- function( design ) {
}

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
