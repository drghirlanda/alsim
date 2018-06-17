library(alsim)

Mod <- newModel( pearce, 0.1 )       # create a pearce model
A  <- c(1,0)                         # create stimulus A
AB <- c(1,1)                         # create stimulus AB
Mod <- phase( Mod, list(A=c(30,1)) ) # train 30 trials with lambda=1
VA <- V( Mod, "A", 0:30 )            # calculate VA across trials
VAB <- V( Mod, "AB" )                # calculate VAB (at trial n)

alsim.options$rem$r <- 0.25 # set replacement factor
Mod <- newModel( rem, .1 )  # create a rem model

A <-  c( 1, 0 ) # create stimuli A, B, and AB
B <-  c( 0, 1 )
AB <- c( 1, 1 )

Mod <- phase(      # train the model with 50 presentations
    Mod,           # of A and 50 of B, all rewarded with a
    list(
	A=c(50,1), # 50 trials with A and reinforcement = 1
	B=c(50,1)  # 50 trials with B and reinforcement = 1 
    )
)

# visualize results:
Vplot( Mod, c("A","B","AB"), 0:50 )

blocking <- list(
    experimental=list(
	phase1=list( A=c(50,1) ),
	phase2=list( AB=c(50,1) )
    ),
    control=list(
	phase2=list( AB=c(50,1) )
    )
)

A <- c(1,0)
B <- c(0,1)
AB <- c(1,1)
model <- newModel( pearce, 0.1 )
results <- experiment( model, blocking, 20 )
Vplot( results$experimental, c("A","B","AB"), 0:100 )

A  <- c(1,0)
B  <- c(0,1)
AB <- c(1,1)
model <- newModel( rem, 0.1 )
alsim.options$rem$r <- 0
featureNegative <- list(
    S=list(
	phase1=list(
	    A=c(50,1),
	    AB=c(50,0)
	)
    )
)
results <- experiment( model, featureNegative, 10 )
Vplot( results$S, c("A","B","AB"), 0:100 )
