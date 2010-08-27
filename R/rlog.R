`rlog` <-function(
  	run, 
	project=getwd(), 
	boot=FALSE, 
	append=TRUE,
	tool='nm6',
	file=filename(project,'CombRunLog.csv'),
	rundir = filename(project, run, if(boot) '.boot' else ''),
	nmlog = file.path(rundir,'NonmemRunLog.csv'),
	nmout = filename(rundir,run,'.lst'),
	pattern=if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
	test='FILE10',
        ...
){
  if(length(run)!=length(unique(run)))stop('run must not contain duplicates')
  if(!append)if(length(file))if(file.exists(file)) file.remove(file)
  specialize <- function(path,run,nm){
  		if(!length(path) %in% c(0,1,length(run)))stop(paste('length of',nm, 'must be 0, 1, or same as run'))
  		if(!length(path))return(path)
  		if(length(path)==1) path <- sapply(run,function(r)gsub('*',r,path,fixed=TRUE))
  		names(path) <- run
  		path
  }
  rundir <- specialize(rundir,run,'rundir')
  nmlog <- specialize(nmlog,run,'nmlog')
  nmout <- specialize(nmout,run,'nmlog')
  #cleanup
  if(length(pattern)){
  		lapply(
  			rundir,
  			function(dir,pattern){
  				if(!file.exists(filename(dir,test)))lapply(
  					pattern,
  					purge.files,
  					dir=dir
  				)
  			},
  			pattern=pattern
  		)
  }
  unilist <- list()
  if(tool=='nm6') unilist <- lapply(
  		nmlog,
  		function(log,...)as.unilog.runlog(
  			as.runlog.file(log,...),
  			tool=tool,
  			...
  		),
  		...
  )
  if(tool=='nm7') unilist <- lapply(
  		run,
  		function(r,filelist,...)as.unilog.run(
  			r,
  			outfile=filelist[[as.character(r)]],
  			tool=tool,
  			...
  		),
  		filelist = nmout,
  		...
  )
  if(length(file)){
  		runloglist <- lapply(unilist,as.runlog.unilog)
  		lapply(
  			runloglist,
  			write.table,
  			file=file,
  			append=TRUE,
  			sep=',',
  			row.names=FALSE,
  			col.names=FALSE,
  			quote=FALSE,
  			na='.'
  		)
  }
  uni <- do.call(rbind,unilist)
  invisible(uni)
}	














