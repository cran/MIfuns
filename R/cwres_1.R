`cwres_1` <-
function (i, ProjectDir){
	#Function to calculate cwres if PLOTR not called (diag=F or diag=0).
    
    #Initialize some variables.
    start <- getwd()
    ndir <- paste(ProjectDir, "/", i, sep = "")
    TabFileName <- paste(ProjectDir, "/", i, ".TAB", sep = "")
    ParFileName <- paste(ProjectDir, "/", i, "par.TAB", sep = "")
    CovFile <- paste(ndir, "/", "cov.datat", sep = "")
    OutFile <- paste(ndir, "/", i,".lst", sep = "")
    
    WorkingDir <- getwd()
    cwtab1 <- paste(ndir, "/cwtab1.deriv", sep = "")
        
    #Test for some prerequisites.
    if (!file.exists(OutFile)) 
        stop(paste("Can't find", OutFile, ". \n For *nix check nmv.p.o* file in ", 
            i, " directory. ", "Working directory is ", getwd(), 
            "."))
    if (!file.exists(TabFileName)) 
        stop(paste("*.TAB file does not exist in ", ProjectDir, 
            ". \n For *nix check nmv.p.o* file in ", i, " directory. ", 
            "Working directory is ", getwd(), ".", sep = ""))
    setwd(ndir)
    #data.file <- read.table(file = DataFileName, header = TRUE, 
    #    sep = ",")
    data.table <- read.table(TabFileName, skip = 1, header = TRUE, 
        as.is = TRUE, comment.char = "")
    data.table$ID <- as.character(data.table$ID)
    if (!file.exists(cwtab1)) 
        message(paste("No CWRES calculated because file cwtab1.deriv not found in NONMEM run directory."))
    if (file.exists(cwtab1)) {
    	# if CWRES already exists in data.table do not recalculate it
    	if(!"CWRES" %in% names(data.table)) {
    		tab.prefix.nm <- paste(ProjectDir,"/", i, "/cwtab",sep="")
    		cwres.all<-compute.cwres(run.number=1,tab.prefix=tab.prefix.nm,printToOutfile=TRUE)
        data.cwres <- read.table(file = paste(ndir, "/cwtab1", 
            sep = ""), skip = 1, header = TRUE)
        data.table$CWRES <- data.cwres$CWRES
        write(paste("Table output from NONMEM for Run ", i, " with CWRES as last column on", format(Sys.time(), "%a %b %d, %X, %Y"), sep=""),file=TabFileName,ncolumns=1)
        suppressWarnings(write.table(data.table, file = TabFileName, 
            sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE, 
            append = TRUE, na = "."))
    	}
    }
    }

