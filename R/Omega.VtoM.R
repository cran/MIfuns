`Omega.VtoM` <-
function(omegaVector) {
	nomega <- round(0.5*(sqrt(1+8*length(omegaVector))-1))
	omegaMatrix <- matrix(0,nomega,nomega)
	pos <- 1
	for( i in 1:nomega) {
	     for(j in 1:i) {
		    omegaMatrix[i,j] <- omegaVector[pos]
		    omegaMatrix[j,i] <- omegaVector[pos]
		    pos <- pos+1
		}
	}
	return(omegaMatrix)
  }

