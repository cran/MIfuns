`runNonmem` <-
function (
	run,
	command,
	project,
	boot,
	urgent,
	checkrunno,
	diag,
	fdata,
	epilog,
	dvname,
	logtrans,
	grp,
	grpnames,
	cont.cov,
	cat.cov,
	par.list,
	eta.list,
	missing,
	invisible,
	checksum,
	grid,
	nice,
	udef,
	compile,
	execute,
	split,
	plotfile=plotfilename(run,project,grp),
	runext = if(boot) '.boot' else if(grid) '.lock' else '',
	rundir = filename(project,run,runext),
	outfile = filename(rundir,run,'.lst'),
	streams = project,
	ctlfile = filename(streams,run,'.ctl'),
	remove  = c('^F[ISRC]','^OU','^nonmem.exe','^nul$',if(fdata)c('^FD','^PR')),
	sync=if(boot)'n'else'y',
	...
){
  #Define some functions.
  final <- function(x)sub('\\.lock','',x)
 
  #Groom arguments.
  rundir <- star(rundir,run)
  ctlfile <- star(ctlfile,run)
  outfile <- star(outfile,run)
  if(!file.exists(ctlfile))message(paste(ctlfile,'was not found'))
  if(!file.exists(ctlfile))return()
  control <- explicitPath(readLines(ctlfile))
  if (checkrunno) writeLines(control <- fixFile(fixProblem(control,run),run),con=ctlfile)
  tabfile <- tabfile(control,dir=final(rundir),...)
  parfile <- parfile(control,dir=final(rundir),...)
  msffile <- msffile(control,dir=final(rundir),...)
  script <- NULL
  epimatch <- try(match.fun(epilog),silent=TRUE)
  if(is.function(epimatch))epilog <- epimatch
  else if (class(epilog)=='character'){
	  script <- epilog
	  epilog <- episcript
  }
  
  #Prepare the file environment.
  if(compile){
  	  if(file.exists(plotfile))file.remove(plotfile)
	  if(file.exists(outfile))file.remove(outfile)
	  if(file.exists(tabfile))file.remove(tabfile)
	  if(file.exists(parfile))file.remove(parfile)
	  if(file.exists(msffile))file.remove(msffile)
	  purge.dir(final(rundir),nice)
	  if(rundir!=final(rundir))purge.dir(rundir)
	  dir.create(rundir, showWarnings = FALSE)
	  dname <- getdname(ctlfile)
	  if(!file.exists(resolve(dname,rundir))){
		  warning(paste(dname,'not visible from',rundir),call.=FALSE,immediate.=TRUE)
		  return()
	  }
	  file.copy(ctlfile, file.path(rundir,basename(ctlfile)), overwrite = TRUE)
  }
  #Run NONMEM.
  runCommand(
  	command=command,
	run=run,
	rdir=rundir,
	boot=boot,
	urgent=urgent,
	checksum=checksum,
	grid=grid,
	udef=udef,
	ctlfile=file.path(rundir,basename(ctlfile)),
	outfile=outfile,
	invisible=invisible,
	compile=compile,
	execute=execute,
	split=split,
	sync=sync,
	...
  )
  #Clean up.
  if(execute){
	  if(sync=='n')return() #because we may have reached here before run is complete.
	  lapply(remove,purge.files,dir=rundir)
	  if(rundir!=final(rundir)){
		dir.create(final(rundir), showWarnings = FALSE)
		file.copy(from=dir(rundir,full.names=TRUE),to=final(rundir),overwrite=TRUE)
		purge.dir(rundir)
	  }
	
	  #Diagnostics
	  #try(runlog(run=run,outfile=outfile,...))
	  if(!udef)
	   if(nmVersion(config(dirname(command))) < 7)
	    try(setCwres(cwres=getCwres(directory=final(rundir)),file=tabfile))
	   #else(try(runlog(run=run,outfile=outfile,...)))
	  if(diag)try(
		PLOTR(
			run=run,
			project=project,
			dvname=dvname,
			logtrans=logtrans,
			grp=grp,
			grpnames=grpnames,
			cont.cov=cont.cov,
			cat.cov=cat.cov,
			par.list=par.list,
			eta.list=eta.list,
			missing=missing,
			ctlfile=ctlfile,
			outfile=final(outfile),
			rundir=final(rundir),
			plotfile=plotfile,
			...
		)
	  )
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
			ctlfile=ctlfile,
			outfile=final(outfile),
			rundir=final(rundir),
			...,
			script=script
		)
	  )
	
	  message(paste("Run ", run, " complete.", sep = ""))
  }
}

#.............................................................................
  purge.dir <- function(dir,nice=FALSE){
  	if(file_test('-d',dir)){
  		files <- dir(dir,full.names=TRUE,all.files=!nice)
  		files <- files[!files %in% grep('\\.$',files,value=TRUE)]
  		if(length(files))file.remove(files)
  		if(!nice)unlink(dir, recursive=TRUE)
  	}
  }
  purge.files <- function(pattern,dir='.'){
  	if(file_test('-d',dir)){
  		files <- dir(dir)
  		files <- grep(pattern,files,value=TRUE,ignore.case=TRUE)
  		if(length(files))file.remove(paste(dir,files,sep='/'))
  	}
  }
   episcript <- function(script,...){
	 extras <- list(...)
	 args <- names(extras)
	 lapply(
	 	args,
		function(x,extras)assign(x,extras[[x]],envir=parent.frame()),
		extras
	)
	try(source(script))
  }
  fixProblem <- function(x,run)sub('(^\\$PROB(LEM)? +(RUN#? *)?)([^ ]+)(.*$)',paste(sep='','\\1',run,'\\5'),x,ignore.case=TRUE)
  fixFile <- function(x,run){
        x <- explicitPath(x)
	risk <- grep('\\bTAB\\b|\\bMSF\\b',x,ignore.case=TRUE)
        except <- grep('\\bMSFI\\b',x,ignore.case=TRUE)
        risk <- setdiff(risk,except)
        dir <- dirname(x)
	base <- basename(x)
	base <- sub('^[^.(par)]+',run,base)
	x[risk] <- file.path(dir[risk],base[risk])
	x
  }
  explicitPath <- function(x){
	risk <- grep('\\.TAB\\b|\\.MSF\\b',x,ignore.case=TRUE)
    	except <- grep('/',x)
    	risk <- setdiff(risk,except)
    	x[risk] <- sub('^(.*\\W)?(\\w*)(\\.msf|\\.tab)(.*)$','\\1./\\2\\3\\4',x[risk],ignore.case=TRUE)
	x
  }
  extractPath <- function(x)sub('(^.*(MSFO?|FILE) *= *)([^ ]*)(.*$)','\\3',x,ignore.case=TRUE)
  resolve <- function(file,dir)ifelse(contains('^\\.',file),file.path(dir,file),file)
  scavenge <- function(expr,lines){
	  x <- lines[grep(expr,lines,ignore.case=TRUE, perl=TRUE)]
	  if(length(x))return(x[[1]]) else return('')
  }
  extfile <- function(ctlfile,dir,extreg,...)resolve(extractPath(scavenge(extreg,ctlfile)),dir)  
  tabfile <- function(ctlfile,dir,tabreg='(?<!par)\\.tab',...)extfile(ctlfile,dir,extreg=tabreg,...)
  parfile <- function(ctlfile,dir,parreg='par\\.tab',...)extfile(ctlfile,dir,extreg=parreg,...)
  msffile <- function(ctlfile,dir,msfreg='^(?!\\$MSFI).*\\.msf',...)extfile(ctlfile,dir,extreg=msfreg,...)
	  
	  
	  
	  
	  
	  
	  
