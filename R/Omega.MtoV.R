`Omega.MtoV` <-
function(omegaMatrix) {
	omegaVector <- NULL
	for( i in 1:dim(omegaMatrix)[1]) {
	     for(j in 1:i) {
		    omegaVector <- c(omegaVector,omegaMatrix[j,i])
		}
	}
	return(omegaVector)
  }

