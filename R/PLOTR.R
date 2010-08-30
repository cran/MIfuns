`PLOTR` <-function(
	run, 
	project=getwd(), 
	rundir=filename(project,run),
	grp = NULL, 
	onefile=TRUE,
	plotfile=plotfilename(run,project,grp,onefile),
	logtrans = FALSE, 
	dvname = 'DV', 
	epilog=NULL,
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99,
	...
){
    
    #process data
    synthesis <- dataSynthesis(
    	run,
	project,
	dvname,
	logtrans,
	grp,
	grpnames,
	cont.cov,
	cat.cov,
	par.list,
	eta.list,
	missing,
	rundir=rundir,
	...
    )
    write.csv(synthesis,filename(rundir,ext='_syn.csv'),row.names=FALSE)
    available <- names(synthesis)
    cont.cov <- strain(cont.cov,available)
    cat.cov <- strain(cat.cov,available)
    par.list <- strain(par.list,available)
    eta.list <- strain(eta.list,available)
    
    
    #open device
    plotfile <- star(plotfile,run)
    safe.call(pdf,file=plotfile,onefile=onefile,...)

    #make plots
    lapply(diagnosticPlots(synthesis, dvname=dvname, group='grpnames', model= paste('Model',run),...),print)
    lapply(covariatePlots(synthesis,cont.cov,cat.cov,par.list,eta.list,...),print)
    lapply(cwresPlots(synthesis,cont.cov,cat.cov,...),print)
    
    #cleanup
    dev.off()
    unlink(filename(rundir,ext='_syn.csv'))
    message(paste('Plotting for run ', run, ' complete.', sep = ''))
    
    #try epilog
    script <- NULL
    epimatch <- try(match.fun(epilog),silent=TRUE)
    if(is.function(epimatch))epilog <- epimatch
    else if (class(epilog)=='character'){
	    script <- epilog
	    epilog <- episcript
    }
    if (!is.null(epilog))if(is.function(epilog))try(
        epilog(
            run=run,
    	    project=project,
	    dvname=dvname,
	    logtrans=logtrans,
	    grp=grp,
	    grpames=grpnames,
	    cont.cov=cont.cov,
	    cat.cov=cat.cov,
	    par.list=par.list,
	    eta.list=eta.list,
	    missing=missing,
	    rundir=rundir,
	    ...,
	    script=script
	)
    )	    
}

#filters elipses for functions that do not accept them
safe.call <- function(what,...){
	extras <- list(...)
	legal <- names(formals(what))
	extras <- extras[names(extras) %in% legal]
	do.call(what=what,args=extras)
}

#creates a filepath from dir,run, and extension
filename <- function(dir,run=NULL,ext=NULL)file.path(dir,paste(run,ext,sep=''))

#calculates a vector of cwres
getCwres <- function(directory){
    	cwrtab1 <- filename(directory, NULL, 'cwtab1.deriv')
        if (!file.exists(cwrtab1)) message(paste(cwrtab1,'does not exist.'))
        if (!file.exists(cwrtab1)) return(NULL)
	tab.prefix <- filename(directory,NULL, '/cwtab')
	cwres.all<-compute.cwres(
		run.number=1,
		tab.prefix=tab.prefix,
		printToOutfile=TRUE
	)
	data.cwres <- read.table(
		file = filename(directory,NULL, '/cwtab1'), 
		skip = 1, 
		header = TRUE
	)
        data.cwres$CWRES
}

#loads non-null cwres onto tab file
setCwres <- function(
	cwres,
	file
){
	if(is.null(cwres))return(NULL)
	cwres <- c('CWRES',cwres)
	tabfile <- readLines(file)[-1]
	if(length(tabfile)!=length(cwres))stop()
	msg <- paste('CWRES added', format(Sys.time(), '%a %b %d, %X, %Y'))
	tabfile <- paste(tabfile,cwres)
	tabfile <- c(msg,tabfile)
	writeLines(tabfile,con = file)
}


#finds the nonmem data set and coverts it to a covariate file
getCovs <- function(file,dir){
	    here <- getwd()
	    setwd(dir)
	    covfile <- NULL
	    try(covfile <- read.table(file = file, header = TRUE, as.is = TRUE, sep = ','))
	    setwd(here)
	    if(is.null(covfile))return(covfile)
	    covfile <- covfile[covfile$C != 'C',]
	    covfile <- covfile[!duplicated(covfile$ID),]
	    return(covfile)
}

#returns the parameter file
getPars <- function(file){ 
	    if (!file.exists(file))warning(paste(filename,'does not exist.'))
	    if (!file.exists(file))return(NULL)
	    file <- read.table(file = file, header = TRUE, skip = 1)
            file <- file[!duplicated(file$ID),]
	    return(file)
}  

#scavenges the data set name/path from the control stream
getdname <- function(filename){
	    datamod <- '^\\$DATA +([^ ]+).*$'
	    control <- scan(file = filename, what = '', comment.char = '', allowEscapes = TRUE, sep = '\n', quiet = TRUE)
	    dablock <- grep('\\$DATA', control, value = TRUE)
	    datfile <- sub(datamod, '\\1', dablock)
	    return(datfile)
}

#finds the tab file and reduces it to observation rows
getTabs <- function(file){
	    tabfile <- read.table(file = file, header = TRUE, as.is = TRUE, skip = 1, comment.char = '')
	    #tabfile$ID <- as.character(tabfile$ID)
	    tabfile <- tabfile[tabfile$EVID == 0, ]
	    tabfile
}   

#melds the grpnames columns into one, renaming levels conditionally
groupnames <- function(data,grp,grpnames=NULL,run){
	    result <- factor(
	    	do.call(
  			paste,
				c(
					as.list(data[,grp,drop=FALSE]),
					sep=", "
				)
			)
		)
	   nlevs <- length(levels(result))
	   if(!is.null(grpnames))if(length(grpnames)==nlevs)levels(result) <- grpnames
	   if(!is.null(grpnames))if(length(grpnames)!=nlevs)warning(
		   paste(
  			"Run", 
			run, 
			"has",
			nlevs,
			"grouping levels but",
			length(grpnames),
			"grpnames (ignored)." 
		)
	  )
	  result
}

#combines any number of tables using left join to load the columns specified in x
synthesis <- function(x,key=character(0),frames,...){
    if(any(sapply(frames,function(x)!inherits(x,'data.frame'))))stop()    
    x <- unique(x)
    y <- frames[[1]]
    frames[[1]] <- NULL
    x <- setdiff(x,names(y))
    while (length(x) & length(frames)){
	    z <- frames[[1]]
	    frames[[1]] <- NULL
	    z <- z[, union(key, intersect(x, names(z))),drop=FALSE]
        z <- z[!duplicated(z[, intersect(names(z), names(y))]),,drop=FALSE]
        if(length(setdiff(names(z),names(y)))) y <- stableMerge(y,z)
	    x <- setdiff(x,names(y))
    }
    y
}

#reduces a character vector to the subset found in options
strain <- function(x,options){
	    if(!is.null(x))x <- intersect(x,options)
            x
}

#back transforms cols in x
backtrans <- function(x,cols){
	    for(col in cols)x[[col]] <- exp(x[[col]])
	    x
}

#generates the plotting data set, given actual data frames, etc.
dataFormat <- function(
	tabfile,
	covfile,
	parfile,
	dvname='DV',
	logtrans=FALSE,
	grp=NULL,
	grpnames=NULL,
	cont.cov=NULL,
	cat.cov=NULL,
	par.list=NULL,
	eta.list=NULL,
	missing=-99,
	run,
	...
){
    if (logtrans) tabfile <- backtrans(tabfile,intersect(names(tabfile),c(dvname,'PRED','IPRE','IPRED')))    
    available <- unique(c(names(tabfile),names(covfile),names(parfile)))
    grp <- strain(grp,available)
    cont.cov <- strain(cont.cov,available)
    cat.cov <- strain(cat.cov,available)
    par.list <- strain(par.list,available)
    eta.list <- strain(eta.list,available)
    #requests <- c(grp,cont.cov,cat.cov,par.list,eta.list)
    findAnywhere <- c(grp,cont.cov,cat.cov)
    findInOutput <- c(par.list,eta.list)
    #synthesis <- synthesis(requests, key='ID',frames=list(tabfile,covfile,parfile),...)
    synthesis <- synthesis(findAnywhere, key='ID',frames=list(tabfile,parfile,covfile),...)
    synthesis <- synthesis(findInOutput, key='ID',frames=list(synthesis,tabfile,parfile),...)
    missing <- as.numeric(as.character(missing))
    for(col in cont.cov) synthesis[[col]] <- as.numeric(as.character(synthesis[[col]]))
    for(col in cont.cov) synthesis[[col]][!is.na(synthesis[[col]]) & synthesis[[col]]==missing] <- NA
    if(is.null(grp))synthesis$grpnames <- 'all'
    if(is.null(grp))grp <- 'grpnames'
    synthesis$grpnames <- groupnames(synthesis,grp,grpnames,run)
    synthesis
}

#generates the plotting data set, given project, run, etc.
dataSynthesis <- function(
	run, 
	project=getwd(), 
	dvname = 'DV',
	logtrans = FALSE,
	grp = NULL, 
	grpnames = NULL,
	cont.cov = NULL,
	cat.cov = NULL,
	par.list = NULL,
	eta.list = NULL,
	missing = -99,
	rundir  = filename(project, run),
	ctlfile = filename(rundir,run,'.ctl'),
	outfile = filename(rundir,run,'.lst'),
	datfile = getdname(ctlfile),
	...
){
    #cleanup arguments
    force(datfile)
    if (!file.exists(outfile)) stop(paste(outfile,'does not exist.'))
    if (!file.exists(ctlfile)) stop(paste(ctlfile,'does not exist.'))
    ctlfile <- readLines(ctlfile)#switch from file name to file content
    tabfile <- tabfile(ctlfile,dir=rundir,...)
    parfile <- parfile(ctlfile,dir=rundir,...)
    if (!file.exists(tabfile)) stop(paste(tabfile,'does not exist.'))
    if (!file.exists(parfile)) stop(paste(parfile,'does not exist.'))

    #acquire data
    tabfile <- getTabs(tabfile)    
    covfile <- getCovs(datfile,rundir)
    parfile <- getPars(parfile)
    
    #process data
    synthesis <- dataFormat(tabfile,covfile,parfile,dvname,logtrans,grp,grpnames,cont.cov,cat.cov,par.list,eta.list,missing,run,...)
    synthesis
}

star <- function(x,y)gsub('*', y, x, fixed=TRUE)
plotfilename=function(
	run,
	dir=getwd(),
	grp=NULL,
	onefile=TRUE,
	stem='DiagnosticPlotReview',
	pext=if(onefile)'.pdf' else '_%03d.pdf',
	...
)filename(
	dir,
	paste(
		stem,
		paste(grp,collapse=''),
		'_',
		run,
		sep=''
	),
	pext
)

