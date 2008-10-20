`PLOTR` <-
function (b, ProjectDir, dvname = NULL, logtrans = FALSE, 
    covplt = FALSE, grp = NULL, grpnames = NULL, cont.cov = NULL, 
    cat.cov = NULL, par.list = NULL, eta.list = NULL, missing = -99) 
{
    #Main Plot function, called independently or by NONR.
    
    #Initialize some variables.
    start <- getwd()
    ndir <- paste(ProjectDir, "/", b, sep = "")
    TabFileName <- paste(ProjectDir, "/", b, ".TAB", sep = "")
    ParFileName <- paste(ProjectDir, "/", b, "par.TAB", sep = "")
    CovFile <- paste(ndir, "/", "cov.datat", sep = "")
    OutFile <- paste(ndir, "/", b, ".lst", sep = "")
    ctl1 <- paste(ndir, "/", b, ".ctl", sep = "")
    ab <- "^\\$DATA +([^ ]+).*$"
    ab1 <- scan(file = ctl1, what = "", comment.char = "", allowEscapes = TRUE, 
        sep = "\n", quiet = TRUE)
    ab2 <- grep("\\$DATA", ab1, value = TRUE)
    DataFileName <- sub(ab, "\\1", ab2)
    missing <- as.numeric(as.character(missing))
    WorkingDir <- getwd()
    cwtab1 <- paste(ndir, "/cwtab1.deriv", sep = "")
    if (is.null(dvname)) {
        dvname <- "DV"
        message("Name of dependent variable is assumed to be 'DV'.")
    }
    
    #Test for some prerequisites.
    if (!file.exists(OutFile)) 
        stop(paste("Can't find", OutFile, ". \n For *nix check nmv.p.o* file in ", 
            b, " directory. ", "Working directory is ", getwd(), 
            "."))
    if (!file.exists(TabFileName)) 
        stop(paste("*.TAB file does not exist in ", ProjectDir, 
            ". \n For *nix check nmv.p.o* file in ", b, " directory. ", 
            "Working directory is ", getwd(), ".", sep = ""))
    setwd(ndir)
    data.file <- read.table(file = DataFileName, header = TRUE, 
        sep = ",", as.is=TRUE)
    data.table <- read.table(TabFileName, skip = 1, header = TRUE, 
        as.is = TRUE, comment.char = "")
    if (!is.null(grp)){
    	available <- union(names(data.file),names(data.table))
    	ignore <- setdiff(grp,available)
        if (length(ignore)) {
            grp <- setdiff(grp,ignore)
            warning(
            	paste(
            		"ignoring grp variable",
            		paste(grp,collapse=", "),
            		"(not exist in *.TAB file or in data set)"
            	)
            )
        }
    }
    data.file.nC <- data.file[data.file$C != "C", ]
    covariates <- data.file.nC[!duplicated(data.file.nC$ID), 
        ]
	
    #Data preparation.
    data.table$ID <- as.character(data.table$ID)
    if (!file.exists(cwtab1)) 
        message(paste("No CWRES plots because file",cwtab1,"not found in NONMEM run directory."))
    if (file.exists(cwtab1)) {
    	# if CWRES already exists in data.table do not recalculate it
    	if(!"CWRES" %in% names(data.table)) {
    		tab.prefix.nm <- paste(ProjectDir,"/", b, "/cwtab",sep="")
    		cwres.all<-compute.cwres(run.number=1,tab.prefix=tab.prefix.nm,printToOutfile=TRUE)
            data.cwres <- read.table(file = paste(ndir, "/cwtab1", sep = ""), skip = 1, header = TRUE)
        data.table$CWRES <- data.cwres$CWRES
        write(paste("Table output from NONMEM for Run ", b, " with CWRES as last column on", format(Sys.time(), "%a %b %d, %X, %Y"), sep=""),file=TabFileName,ncolumns=1)
        suppressWarnings(write.table(data.table, file = TabFileName, 
            sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE, 
            append = TRUE, na = "."))
    	}
    }
    dataObs <- data.table[data.table$EVID == 0, ]
    if (!is.null(grp)) {
    	need <- grp[!grp %in% names(dataObs)]
    	have <- grp[grp  %in% names(covariates)]
    	can <- intersect(need,have)
        if (length(can)) dataObs <- merge(
        	dataObs, 
        	covariates[, c("ID", can)], 
        	by = "ID"
        )
    }   
    if (logtrans) {
        dataObs$DV <- exp(dataObs$DV)
        dataObs$PRED <- exp(dataObs$PRED)
        dataObs$IPRE <- exp(dataObs$IPRE)
    }
    pdf(
    paste(ProjectDir, "/","DiagnosticPlotReview_", b,".pdf",sep=""),
  	height=6,
  	width=6
  )
    #Do the standard diagnostic plots.
    lapply(
    	diagnostics(grp, grpnames, ProjectDir, b, dataObs, dvname, covplt),
    	print
    )
	
    #Consider covariate plots.
    if (!covplt) message(paste("No Covariate Plots requested for Run ", b, ".", sep = ""))
    if (covplt & !file.exists(ParFileName)) warning(paste("Cov. plots requested but can't find",ParFileName))
    if (covplt & !file.exists(TabFileName)) warning(paste("Cov. plots requested but can't find",TabFileName))
    if (covplt & file.exists(ParFileName) & file.exists(TabFileName))lapply(
        doCov(ParFileName, par.list, eta.list, CovFile, ProjectDir, b, cont.cov, cat.cov, covariates, dataObs,missing),
        print
    )
    dev.off()
    message(paste("Plotting for run ", b, " complete.", sep = ""))
    setwd(start)
}


























