`ind.cwres` <-
function (ind.data, H.names, G.names, OMEGA, SIGMA, IND.ETAS, 
    ...) 
{
    if (is.null(ind.data$MDV)) {
        ind.data1 <- ind.data
    }
    else {
        ind.data1 <- ind.data[ind.data$MDV == 0, ]
    }
    if (nrow(ind.data1) != 0) {
        H.EPS = as.matrix(subset(ind.data1, select = H.names))
        G.ETA = as.matrix(subset(ind.data1, select = G.names))
        TMP <- diag(H.EPS %*% SIGMA %*% t(H.EPS))
        IND.COV = diag(TMP, nrow = length(TMP)) + G.ETA %*% OMEGA %*% 
            t(G.ETA)
        EXP.F <- as.matrix(ind.data1$IPRE) - G.ETA %*% IND.ETAS
        FOCE.RES <- as.matrix(ind.data1$DV) - EXP.F
        SQRT.IND.COV <- sqrtm(IND.COV)
        IND.CWRES <- solve(SQRT.IND.COV, FOCE.RES)
        if (is.null(ind.data$MDV)) {
        }
        else {
            CWRES <- rep(0, length(ind.data[, 1]))
            ind.data2 <- cbind(ind.data, CWRES)
            ind.data2[ind.data2$MDV == 0, "CWRES"] <- IND.CWRES
            IND.CWRES <- as.matrix(ind.data2["CWRES"])
        }
    }
    else {
        CWRES <- rep(0, length(ind.data[, 1]))
        ind.data2 <- cbind(ind.data, CWRES)
        IND.CWRES <- as.matrix(ind.data2["CWRES"])
    }
    return(IND.CWRES)
}

