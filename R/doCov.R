`doCov` <-
function (ParFileName, par.list, eta.list, CovFile, 
    ProjectDir, b, cont.cov, cat.cov, covariates, dataObs, missing) 
{
	plots <- list()
    data <- dataObs
    cont <- cont.cov
    cat <- cat.cov
    parfile <- read.table(file = ParFileName, header = TRUE, skip = 1)
    parfile <- parfile[!duplicated(parfile$ID),]
    par.list <- intersect(par.list,names(parfile))
    eta.list <- intersect(eta.list,names(parfile))
    cont <- intersect(cont, names(covariates))
    cat <- intersect(cat, names(covariates))
    if(length(union(par.list,eta.list)))covariates <- stableMerge(
    	covariates, 
    	parfile[, c("ID",union(par.list,eta.list))]
    )
    write.table(
    	covariates, 
        file = CovFile, 
        sep = ",", 
        quote = FALSE, 
        row.names = FALSE, 
        col.names = TRUE, 
        append = FALSE, 
        na = "."
    )
    for(col in cont){
    	covariates[[col]] <- as.numeric(as.character(covariates[[col]]))
    	covariates[[col]][!is.na(covariates[[col]]) & covariates[[col]]==missing] <- NA
    }
    #Covariate SPLOM
    if (length(cont) >= 2)plots$covSplom <- splom(
    	covariates[, cont], 
    	panel = function(x, y) {
        	panel.splom(x, y)
            panel.loess(x,y)
        },
        main="Covariate Scatterplots",
        xlab=""
    )
    #Cont vs cat bwpots
    if (length(cont) & length(cat)) {
        molten <- melt(covariates,measure.var=cont,id.var=cat)
        names(molten)[names(molten)=="variable"] <- "cont"
    	names(molten)[names(molten)=="value"] <- "y"
    	plasma <- melt(molten,measure.var=cat)
    	names(plasma)[names(plasma)=="variable"] <- "cat"
    	names(plasma)[names(plasma)=="value"] <- "x"
    	plots$contCat <- bwplot(
    		y ~ as.factor(x) | cont + cat,
    		plasma,
    		as.table=TRUE,
    		layout=c(2,2),
    		horizontal=FALSE,
    		ylab="continuous covariate",
    		xlab="categorical covariate",
    		scales=list(y=list(relation="free")),
    		main="Continuous Covariates vs. Categorical Covariates"
    	)
    }
    #ETA SPLOM
	if (length(eta.list) >= 2) {
        plots$etaSplom <- splom(
        	covariates[, eta.list], 
        	panel = function(x, y) {
            	panel.splom(x, y)
            	panel.loess(x,y)
        	},
        	main="ETA Scatterplots",
        	xlab=""
        )
    }
    #Parmater SPLOM
    if (length(par.list) >= 2) {
        plots$paramSplom <- splom(
        	covariates[, par.list], 
        	panel = function(x, y) {
            	panel.splom(x, y)
            	panel.loess(x,y)
        	},
        	main="Parameter Scatterplots",
        	xlab=""
        )
    }
    #ETA Histograms
    if(length(eta.list)){
    	etas <- melt(covariates,measure.var=eta.list)
    	plots$etaHist <- histogram(
    		~ value | variable,
    		etas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Histograms of Etas",
    		scales=list(relation="free")
    	)
	}
    #ETA vs Categoricals
    if(length(cat) && length(eta.list)){
    	etas <- melt(covariates,measure.var=eta.list,id.var=cat)
    	names(etas)[names(etas)=="variable"] <- "eta"
    	names(etas)[names(etas)=="value"] <- "delta"
    	condEtas <- melt(id.var=c("eta","delta"),etas)
    	plots$etaCat <- bwplot(
    		delta ~ as.factor(value) | variable + eta,
    		condEtas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Boxplots of Etas by Categorical Covariate",
    		horizontal=FALSE,
    		scales=list(relation="free"),
    		ylab="ETA",
    		xlab="categorical covariate level"
    	)
    }
    #ETAS vs. Continuous
    if (length(cont) && length(eta.list)) {
    	etas <- melt(covariates,measure.var=eta.list,id.var=cont)
    	names(etas)[names(etas)=="variable"] <- "eta"
    	names(etas)[names(etas)=="value"] <- "delta"
    	condEtas <- melt(id.var=c("eta","delta"),etas)
    	plots$etaCont <- xyplot(
    		delta ~ value | variable + eta,
    		condEtas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Etas vs. Continuous Covariates",
    		ylab="ETA",
    		xlab="continuous covariate",
    		scales=list(relation="free"),
    		panel=function(x,y,...){
    			panel.xyplot(x,y,...)
    			panel.abline(h=0)
    			panel.loess(x,y,lty=2,col="red",...)
    		}
    	)
    }
    #CWRES
    if(length(union(cont,cat)))data <- stableMerge(data,covariates[,c("ID",union(cont,cat))])
    #CWRES vs. Categoricals
    if("CWRES" %in% names(data) && length(cat)){
    	res <- melt(data,id.var="CWRES",measure.var=cat)
    	plots$cwresCat <- bwplot(
    		CWRES ~ as.factor(value) | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="CWRES vs. Categorical Covariates",
    		xlab="categorical Covariate",
    		ylab="conditional weighted residuals",
    		scales=list(relation="free")
    	)
    }
    #CWRES vs. Continuous
    if("CWRES" %in% names(data) && length(cont)){
    	res <- melt(data,id.var="CWRES",measure.var=cont)
    	plots$cwresCont <- xyplot(
    		CWRES ~ value | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="CWRES vs. Continuous Covariates",
    		xlab="continuous covariate",
    		ylab="conditional weighted residuals",
    		scales=list(relation="free"),
    		panel=function(x,y,...){
    			panel.xyplot(x,y,...)
    			panel.abline(h=0)
    			panel.loess(x,y,lty=2,col="red",...)
    		}
    	)
    }
    if (file.exists(CovFile)) file.remove(CovFile)
    plots
}

