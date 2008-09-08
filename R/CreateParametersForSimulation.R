`CreateParametersForSimulation` <-
function(nsim,ThetaMean,ThetaCovar,
         OmegaModeList,OmegaDfList,SigmaModeList,SigmaDfList,digits=4) {	

  parameters <- NULL
  ntheta <- length(ThetaMean)
  if(ntheta == 1 & length(ThetaCovar)!= 1) stop("Incorrect THETA input")
  if(ntheta != 1) if(ntheta != dim(ThetaCovar)[1] |
     ntheta != dim(ThetaCovar)[2] ) stop("Incorrect THETA input")
  if(det(as.matrix(ThetaCovar)) < 0) stop("Input Error: ThetaCovar is not positive-definite.")
  if(!is.list(OmegaModeList)) OmegaModeList <- list(OmegaModeList)
  if(!is.list(SigmaModeList)) SigmaModeList <- list(SigmaModeList)

  parameters <- cbind(parameters,mvrnorm(nsim,ThetaMean,ThetaCovar))

  nOmegaBlocks <- length(OmegaDfList)
  for(iomega in 1:nOmegaBlocks) {
      omegaMode <- OmegaModeList[[iomega]]
      omegaDf <-  OmegaDfList[iomega]
      nomega <- length(omegaMode)
      if(nomega != 1) if(dim(omegaMode)[1] != dim(omegaMode)[2] ) stop("Incorrect OMEGA input")
      if(det(as.matrix(omegaMode)) < 0) stop(paste("OmegaModeList[[",iomega,
                                 "]] is not positive definite",sep=""))
      if(omegaDf < nomega) stop("Input Error: OMEGA DF value is less than matrix dimension.")
      if(nomega > 1) {
	     parameters <- cbind(parameters, SimulateOmega(nsim,omegaDf,omegaMode))
	} else {
 	     parameters <- cbind(parameters, rinvchisq(nsim,omegaDf,omegaMode))
	}   
  }

  nSigmaBlocks <- length(SigmaDfList)
  for(isigma in 1:nSigmaBlocks) {
      sigmaMode <- SigmaModeList[[isigma]]
      sigmaDf <-  SigmaDfList[isigma]
      nsigma <- length(sigmaMode)
      if(nsigma != 1) if(dim(sigmaMode)[1] != dim(sigmaMode)[2] ) stop("Incorrect SIGMA input")
      if(det(as.matrix(sigmaMode)) < 0) stop(paste("SigmaModeList[[",iomega,
                                 "]] is not positive definite",sep=""))
      if(sigmaDf < nsigma) stop("Input Error: SIGMA DF value is less than matrix dimension.")
      if(nsigma > 1) {
	     parameters <- cbind(parameters, SimulateOmega(nsim,sigmaDf,sigmaMode))
	} else {
 	     parameters <- cbind(parameters, rinvchisq(nsim,sigmaDf,sigmaMode))
	}   
  }
	
   return(round(signif(parameters,digits),6))
}

