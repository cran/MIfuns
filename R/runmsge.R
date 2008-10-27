`runmsge` <-
function (
	NMcom, ProjectDir, i, boot, concurrent, Platform, SGEflgs, 
    checkrunno, diag, fdata, epilog, dvname, logtrans, 
    covplt, grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
    missing, dosbox, nochecksum, grid, nice
){

  #Set NONMEM output directory.
  origin <- getwd()
  ndir <- paste(ProjectDir, "/", i, sep = "")
  bootstrap <- boot %in% c(1,3)
  
  #Set runtime directory.
  if (concurrent & !bootstrap) rdir <- paste(ProjectDir, "/", i, ".lock", sep = "")
  if (concurrent & bootstrap) rdir <- paste(ProjectDir, "/", i, ".boot", sep = "")
  if ((Platform %in% c("Windows","Mac")) & grid==FALSE) rdir <- ndir
  if (!concurrent & (Platform != "Windows" & Platform != "Mac")) stop("rdir not set for !concurrent/Nix")
  
  #Name both copies of the control stream.
  ctl1 <- paste(ProjectDir, "/", i, ".ctl", sep = "")
  ctl2 <- paste(      rdir, "/", i, ".ctl", sep = "")
  
  #Groom the control stream.
  if (checkrunno) {
    a <- scan(file = ctl1, what = "", comment.char = "", allowEscapes = TRUE, sep = "\n", quiet = TRUE)
    a <- sub("#.[0-9]{1,4}", paste("# ", i, sep = ""), a)
    a <- gsub("E=../[0-9]{1,4}", paste("E=../", i, sep = ""), a)
    a <- gsub("O=../[0-9]{1,4}", paste("O=../", i, sep = ""), a)
    write.table(a, file = ctl1, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  
  #Define some functions.
  contains <- function (pattern, text, ...){
    hits <- regexpr(pattern, text, ...)
    hits >= 0
  }
  purge.dir <- function(dir,nice=FALSE){
  	if(file_test("-d",dir)){
  		files <- dir(dir,full.names=TRUE,all.files=!nice)
  		files <- files[!files %in% grep("\\.$",files,value=TRUE)]
  		if(length(files))file.remove(files)
  		if(!nice)file.remove(dir)
  	}
  }
  purge.files <- function(pattern,dir="."){
  	if(file_test("-d",dir)){
  		files <- dir(dir)
  		files <- grep(pattern,files,value=TRUE,ignore.case=TRUE)
  		if(length(files))file.remove(files)
  	}
  }
  
  #Prepare the file environment.
  setwd(ProjectDir) 
  purge.files(paste("^",i,"[^0-9]*\\.TAB$",sep=""))
  purge.files(paste("^[^0-9]*",i,"\\.PDF$",sep=""))
  purge.dir(ndir,nice)
  if(contains("\\.lock$",rdir))purge.dir(rdir)
  dir.create(rdir, showWarnings = FALSE)
  file.copy(ctl1, ctl2, overwrite = TRUE)
  setwd(rdir)
  
  #Run NONMEM.
  runnm(NMcom, i, boot, concurrent, Platform, SGEflgs, dosbox, nochecksum, grid)
  
  #Clean up (if not bootstrap run).
  if(!bootstrap){
  	purge.files("^F[ISRC].*")
  	purge.files("^OU.*")
  	purge.files("nonmem.exe")
  	if(!fdata)purge.files("^FD*")
  	if(!fdata)purge.files("^PR*")
  	Sys.chmod(grep(paste("^",i,"\\.",sep=""),dir(),value=TRUE),mode="0664")
  	Sys.chmod(grep("n.*\\.",dir(),value=TRUE),mode="0664")
  	try(file.rename(grep("^Run",dir(),value=TRUE),"nonmem.log"),silent=TRUE)
  	if(contains("\\.lock$",rdir)){ 
  		dir.create(ndir, showWarnings = FALSE)
  		file.copy(from=dir(),to=ndir,overwrite=TRUE)
  		setwd(ndir)
  		purge.dir(rdir)
  	}
  	Sys.chmod(dir(), mode="0664")
  	setwd("..")
  	Sys.chmod(grep("\\.TAB$",dir(),value=TRUE), mode="0664")
  	Sys.chmod(grep("\\.MSF$",dir(),value=TRUE), mode="0664")
  }
  
  #Diagnostics
  if(diag & !bootstrap)try(
    	PLOTR(
    		i, ProjectDir, dvname, logtrans, covplt, 
            grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
            missing
        )
    )
  if(!diag & !bootstrap)try(cwres_1(i, ProjectDir))   
  if (!is.null(epilog))try(source(epilog, local = TRUE, print.eval = TRUE))
  setwd(origin)
}

