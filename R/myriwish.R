`myriwish` <-
function(s,df,Cov) {
	nu <- df - (s-1)
	iwishV <- Omega.MtoV(CheckPositiveMatrix(riwish(s,nu,df*Cov)))
   return(iwishV)
}

