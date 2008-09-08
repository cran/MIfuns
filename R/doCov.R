`doCov` <-
function (ParFileName, par.list, eta.list, CovFile, 
    ProjectDir, b, cont.cov, cat.cov, covariates, dataObs, missing) 
{
    #This function handles the Covariate plotting
    grph.cwres <- "CWRES" %in% names(dataObs)
    parfile <- read.table(file = ParFileName, header = TRUE, 
        skip = 1)
    parfile.i <- parfile[!duplicated(parfile$ID), ]
    p.names <- names(parfile.i)
    par.eta <- c("ID", par.list, eta.list)
    par.eta <- intersect(par.eta, p.names)
    par.list <- intersect(par.eta, par.list)
    eta.list <- intersect(par.eta, eta.list)
    if(!is.null(cont.cov)) cont.cov <- intersect(cont.cov, names(covariates))
    if(!is.null(cat.cov)) cat.cov <- intersect(cat.cov, names(covariates))
    if (!all(par.eta %in% p.names)) {
        warning(paste("Some parameters or etas missing from *par.TAB file for Run ", 
            b, ".", sep = ""))
    }
    else {
        if (any(par.eta != "ID")) 
            cov.data <- merge(covariates, parfile.i[, par.eta], 
                by = "ID")
        write.table(cov.data, file = CovFile, sep = ",", quote = FALSE, 
            row.names = FALSE, col.names = TRUE, append = FALSE, 
            na = ".")
    }
#    startDevice(ProjectDir, "DiagnosticPlotReviewCov", b)
    if (length(cont.cov) >= 2) {
        for (k in 1:length(cont.cov)) {
            if (k == 1) 
                z <- cov.data
            col <- cont.cov[[k]]
            if (!col %in% names(z)) 
                warning(paste(col, "not a valid column name."))
            z[[col]] <- as.numeric(as.character(z[[col]]))
            z <- z[z[[col]] != missing, ]
        }
        cov.data.r <- z
        pairs(cov.data.r[, cont.cov], panel = function(x, y) {
            points(x, y)
            lines(lowess(x, y))
        })
    }
    if (!is.null(cont.cov) & !is.null(cat.cov)) {
        y <- cov.data
        for (j in 1:length(cont.cov)) y[[cont.cov[j]]] <- as.numeric(as.character(y[[cont.cov[j]]]))
        cov.data <- y
        par(mfrow = c(2, 2))
        for (k in 1:length(cont.cov)) {
            z <- cov.data
            col <- cont.cov[[k]]
            if (!col %in% names(z)) 
                warning(paste(col, "not a valid column name."))
            z[[col]] <- as.numeric(as.character(z[[col]]))
            z <- z[z[[col]] != missing, ]
            cov.data.r <- z
            for (j in 1:length(cat.cov)) {
                plot(x = as.factor(cov.data.r[, cat.cov[j]]), 
                  y = as.double(cov.data.r[, cont.cov[k]]), xlab = cat.cov[j], 
                  ylab = cont.cov[k])
            }
        }
    }
    par(mfrow = c(1, 1))
    if (length(eta.list) >= 2) {
        pairs(cov.data[, eta.list], panel = function(x, y) {
            points(x, y)
            lines(lowess(x, y))
        })
    }
    if (length(par.list) >= 2) {
        pairs(cov.data[, par.list], panel = function(x, y) {
            points(x, y)
            lines(lowess(x, y))
        })
    }
    z <- as.factor(eta.list)
    if (length(z) > 0) {
        par(mfrow = c(2, 1))
        for (k in 1:length(eta.list)) {
            hist(cov.data[, eta.list[k]], xlab = eta.list[k], 
                breaks = 20, main = paste("Histogram of ", eta.list[k], 
                  sep = ""))
        }
    }
    if (!is.null(cat.cov)) {
        par(mfrow = c(2, 2))
        for (k in 1:length(eta.list)) {
            for (j in 1:length(cat.cov)) {
                plot(x = as.factor(cov.data[, cat.cov[j]]), y = as.double(cov.data[, 
                  eta.list[k]]), xlab = cat.cov[j], ylab = eta.list[k])
            }
        }
    }
    if (!is.null(cont.cov)) {
        par(mfrow = c(2, 2))
        for (k in 1:length(eta.list)) {
            for (j in 1:length(cont.cov)) {
                x <- cov.data
                x[[cont.cov[j]]] <- as.numeric(as.character(x[[cont.cov[j]]]))
                x <- x[x[[cont.cov[j]]] != missing & !is.na(x[[cont.cov[j]]]), 
                  ]
                cov.data.r <- x
                plot(x = as.double(cov.data.r[, cont.cov[j]]), 
                  y = as.double(cov.data.r[, eta.list[k]]), xlab = cont.cov[j], 
                  ylab = eta.list[k])
                lines(lowess(x = as.double(cov.data.r[, cont.cov[j]]), 
                  y = as.double(cov.data.r[, eta.list[k]])))
                abline(h = 0, lty = 2)
            }
        }
    }
    
    #Conditionally, graph CWRES.
    if (grph.cwres) {
    	if(!is.null(cont.cov) | !is.null(cat.cov)){
    		
        all.cov <- c(cont.cov, cat.cov)
        c.names <- names(dataObs)
        all.cov.miss <- setdiff(all.cov, c.names)
        all.cov.miss <- c("ID", all.cov.miss)
        a <- dim(dataObs)[1]
		if(length(all.cov.miss) > 1) dataObs <- merge(dataObs, covariates[, all.cov.miss], by = "ID")
        b <- dim(dataObs)[1]
        if (!all(a == b)) 
            warning("Problem with merge of CWRES and covariates; check plots.")
        y <- dataObs
		if(!is.null(cont.cov)) for (j in 1:length(cont.cov)) y[[cont.cov[j]]] <- as.numeric(as.character(y[[cont.cov[j]]]))
        dataObs <- y
        eta.list <- c("CWRES")
        eta.name <- c("Conditional Weighted Residuals")
        par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mar = c(4.1, 
            4.1, 1.5, 0.8), cex.axis = 0.8)
        if (!is.null(cat.cov)) {
            for (k in 1:length(eta.list)) {
                for (j in 1:length(cat.cov)) {
                  plot(x = as.factor(dataObs[, cat.cov[j]]), 
                    y = as.double(dataObs[, eta.list[k]]), xlab = cat.cov[j], 
                    ylab = eta.name[k], col = "lightgrey")
                  abline(h = 0)
                }
            }
        }
        if (!is.null(cont.cov)) {
            for (k in 1:length(eta.list)) {
                for (j in 1:length(cont.cov)) {
                  x <- dataObs
                  x <- x[x[[cont.cov[j]]] != missing, ]
                  cov.data.r <- x
                  plot(x = as.double(cov.data.r[, cont.cov[j]]), 
                    y = as.double(cov.data.r[, eta.list[k]]), 
                    xlab = cont.cov[j], ylab = eta.name[k])
                  lines(lowess(x = as.double(cov.data.r[, cont.cov[j]]), 
                    y = as.double(cov.data.r[, eta.list[k]])))
                  abline(h = 0, lty = 2)
                }
            }
        }
	  }	
	}
    
    
    
    stopDevice()
    if (file.exists(CovFile)) 
        file.remove(CovFile)
}

