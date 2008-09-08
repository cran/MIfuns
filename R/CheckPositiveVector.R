`CheckPositiveVector` <-
function(omegaVector) {
	omegaMatrix <- Omega.VtoM(omegaVector)
	omegaMatrix <- CheckPositiveMatrix(omegaMatrix)
	return(Omega.MtoV(omegaMatrix))
}

