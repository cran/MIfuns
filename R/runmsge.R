`runmsge` <-
function (NMcom, ProjectDir, i, boot, concurrent, Platform, SGEflgs, 
            checkrunno, diag, fdata, epilog, dvname, logtrans, 
            covplt, grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
            missing, dosbox, nochecksum, grid, nice) 
{
  delim <- "/"
  ndir <- paste(ProjectDir, delim, i, sep = "")
  if (concurrent & (boot == 0 | boot == 2)) 
    rdir <- paste(ProjectDir, delim, i, ".lock", sep = "")
  if (concurrent & (boot == 1 | boot == 3)) 
    rdir <- paste(ProjectDir, delim, i, ".boot", sep = "")
  if ((Platform == "Windows" | Platform == "Mac") & grid==FALSE) 
    rdir <- ndir
  if (!concurrent & (Platform != "Windows" & Platform != "Mac")) 
    stop("rdir not set for !concurrent/Nix")
  ctl1 <- paste(ProjectDir, delim, i, ".ctl", sep = "")
  ctl2 <- paste(rdir, delim, i, ".ctl", sep = "")
  if (checkrunno) {
    a <- scan(file = ctl1, what = "", comment.char = "", 
              allowEscapes = TRUE, sep = "\n", quiet = TRUE)
    a <- sub("#.[0-9]{1,4}", paste("# ", i, sep = ""), a)
    a <- gsub("E=../[0-9]{1,4}", paste("E=../", i, sep = ""), 
              a)
    a <- gsub("O=../[0-9]{1,4}", paste("O=../", i, sep = ""), 
              a)
    write.table(a, file = ctl1, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
  ab <- "^\\$DATA +([^ ]+).*$"
  ab1 <- scan(file = ctl1, what = "", comment.char = "", allowEscapes = TRUE, 
              sep = "\n", quiet = TRUE)
  ab2 <- grep("DATA", ab1, value = TRUE)
  DataFile <- sub(ab, "\\1", ab2)
  if (Platform == "Nix" | Platform == "Mac") {
    setwd(ProjectDir)
    oldtab <- paste("rm -rf ", i, ".TAB ", i, "*.TAB", sep = "")
    system(oldtab)
    oldpdf <- paste("rm -rf *", i, ".pdf", sep = "")
    system(oldpdf)
    if(nice){
    	delcon1 <- paste("rm -rf ", ndir,"/cwtab*",sep="")
    	delcon2 <- paste("rm -rf ", ndir,"/*.*",sep="")
    	system(delcon1)
    	system(delcon2)
    	delcon3 <- paste("rm -rf ", rdir,"/cwtab*",sep="")
    	delcon4 <- paste("rm -rf ", rdir,"/*.*",sep="")
    	system(delcon3)
    	system(delcon4)    	
    }else
    { 
    deldir <- paste("rm -rf ", ndir, sep = "")#'nice' target
    system(deldir)
    delrdir <- paste("rm -rf ", rdir, sep= "")#'nice' target
    system(delrdir)
    }
    dir.create(rdir, showWarnings = FALSE)
    file.copy(ctl1, ctl2, overwrite = TRUE)
    setwd(rdir)
  }
  if (Platform == "Windows") {
    setwd(ProjectDir)
    oldtab <- paste("cmd /C del /F /Q ", i, ".TAB ", i, "*.TAB", 
                    sep = "")
    system(oldtab)
    oldpdf <- paste("cmd /C del /F /Q *", i, ".pdf", sep = "")
    system(oldpdf)
    if(nice){
    	setwd(ndir)
    	cl.dir <- paste("cmd /C del *", 
                     sep = "")#'nice' target
        message(cl.dir)
        system(cl.dir)  
        setwd(ProjectDir)  	
    }else
    {
    del.dir <- paste("cmd /C rmdir /S /Q ", "\"", ndir, "\"", 
                     sep = "")#'nice' target
    message(del.dir)
    system(del.dir)
    }
    dir.create(ndir, showWarnings = FALSE)#'nice' target
    file.copy(ctl1, ctl2, overwrite = TRUE)
    setwd(rdir)
  }
  runnm(NMcom, i, boot, concurrent, Platform, SGEflgs, dosbox, nochecksum, grid)
  if (!diag) 
    message("Diagnostic plots not requested.")
  if (Platform == "Nix") {
    if (concurrent & (boot == 0 | boot == 2)) {
      delnon <- paste("rm -rf FI* FS* FR* OU* nonm* FC*")
      system(delnon)
      if (!fdata) {
        delfd <- paste("rm -rf FD* PR*")
        system(delfd)
      }
      perm2 <- paste("chmod 664 ", rdir, "/", i, ".*", 
                     sep = "")
      system(perm2)
      perm2a <- paste("chmod 664 ", rdir, "/*n*.*", sep = "")
      try(system(perm2a))
      mv.log <- paste("mv ",rdir,"/Run* ","nonmem.log",sep="")
      try(system(mv.log))
      #if(!diag)
      #   try(cwres_1(i, rdir))
      mv.lock <- paste("mv ", rdir, " ", ndir, sep = "")
      system(mv.lock)
      #setwd(ndir)
      if(!diag)
         try(cwres_1(i, ProjectDir))
      perm3 <- paste("chmod 664 ", ProjectDir, "/", i, 
                     "/*.*", sep = "")
      system(perm3)
      perm4 <- paste("chmod 664 ", ProjectDir, "/*.TAB", 
                     sep = "")
      system(perm4)
      perm5 <- paste("chmod 664 ", ProjectDir, "/*.MSF", 
                     sep = "")
      system(perm5)
      if (diag) 
        try(PLOTR(i, ProjectDir, dvname, logtrans, covplt, 
                  grp, grpnames, cont.cov, cat.cov, par.list, 
                  eta.list, missing))
    }
    if (!is.null(epilog)) 
      try(source(epilog, local = TRUE, print.eval = TRUE))
    #if (nocwres) {
    #  delcwres <- paste("rm -rf ", ProjectDir, "/", i, 
    #                    "/cwtab*", sep = "")
    #  system(delcwres)
    #}
  }
  if (Platform == "Mac" & grid==FALSE) {
    delnon <- paste("rm -rf FI* FS* FR* OU* nonm* fs* FC*")
    system(delnon)
    if (!fdata) {
      delfd <- paste("rm -rf FD* PR*")
      system(delfd)
    }
    if (diag) 
      try(PLOTR(i, ProjectDir, dvname, logtrans, covplt, 
                grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
                missing))
   if(!diag)
         try(cwres_1(i, ProjectDir))            
    if (!is.null(epilog)) 
      try(source(epilog, local = TRUE, print.eval = TRUE))
    #if (nocwres) {
    #  delcwres <- paste("rm -rf ", ProjectDir, "/", i, 
    #                    "/cwtab*", sep = "")
    #  system(delcwres)
    #}
  }
   if (Platform == "Mac" & grid==TRUE) {
    if (concurrent & (boot == 0 | boot == 2)) {
    	delnon <- paste("rm -rf FI* fs* FS* FR* OU* nonm* FC*")
      system(delnon)
      if (!fdata) {
        delfd <- paste("rm -rf FD* PR* fs*")
        system(delfd)
      }
      perm2 <- paste("chmod 664 ", rdir, "/", i, ".*", 
                     sep = "")
      system(perm2)
      perm2a <- paste("chmod 664 ", rdir, "/*n*.*", sep = "")
      try(system(perm2a))
      mv.log <- paste("mv ",rdir,"/Run* ","nonmem.log",sep="")
      try(system(mv.log))
      if(nice){
      	mv.lock <- paste("mv ", rdir, "/* ", ndir, sep = "")
      	rm.lock <- paste("rm -rf ",rdir, sep="")
      	system(mv.lock)
      	system(rm.lock)
      }else
      {
      	mv.lock <- paste("mv ", rdir, " ", ndir, sep = "")
      	system(mv.lock)
      }
      setwd(ndir)
      perm3 <- paste("chmod 664 ", ProjectDir, "/", i, 
                     "/*.*", sep = "")
      system(perm3)
      perm4 <- paste("chmod 664 ", ProjectDir, "/*.TAB", 
                     sep = "")
      system(perm4)
      perm5 <- paste("chmod 664 ", ProjectDir, "/*.MSF", 
                     sep = "")
      system(perm5)
    if (diag) 
      try(PLOTR(i, ProjectDir, dvname, logtrans, covplt, 
                grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
                missing))
   if(!diag)
         try(cwres_1(i, ProjectDir))            
    if (!is.null(epilog)) 
      try(source(epilog, local = TRUE, print.eval = TRUE))
    }
    #if (nocwres) {
    #  delcwres <- paste("rm -rf ", ProjectDir, "/", i, 
    #                    "/cwtab*", sep = "")
    #  system(delcwres)
    #}
  }
  
  if (Platform == "Windows") {
  	if(!diag)
         try(cwres_1(i, ProjectDir))   
    if (diag) 
      try(PLOTR(i, ProjectDir, dvname, logtrans, covplt, 
                grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
                missing))
    if (!is.null(epilog)) 
      try(source(epilog, local = TRUE, print.eval = TRUE))
    delnon <- paste("cmd /C del /F /Q FI* FS* FR* OU* FC* nonmem.exe ")
    system(delnon)
    if (!fdata) {
      delfd <- paste("cmd /C del /F /Q FD* PR*")
      system(delfd)
    }
    #if (nocwres) {
    #  delcwres <- paste("cmd /C del /F /Q cwtab*")
    #  system(delcwres)
    #}
  }
}

