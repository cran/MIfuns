`CheckPositiveMatrix` <-
function(PosMat) {
     if(any(diag(PosMat) <=0)) stop("OMEGA matrix cannot be made positive definite")
     n <- length(diag(PosMat))
     for(i in 1:n) for(j in 1:n) {
	       temp <-PosMat[i,j]
	       n1 <- nchar(abs(round(temp)))
	       if(n1 > 6) stop("Element of the matrix > 1E+07")
	       n2 <- 6 - n1
	       temp <- temp*10^n2
	       if(i == j) temp <- round(temp)/10^n2
	       if(i != j) temp <- floor(temp)/10^n2
	       PosMat[i,j] <- temp          
	 }
	 if(det(PosMat) <=0) {
		PosMat <- 0.97*PosMat + 0.03*diag(diag(PosMat))
       PosMat <- CheckPositiveMatrix(PosMat)
    }	
    return(PosMat)	
}

