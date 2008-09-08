`NONR` <-  
function (NMcom, b, ProjectDir, boot = 0, concurrent = TRUE, 
    SGEflgs = "", checkrunno = TRUE, diag = TRUE, fdata = FALSE, 
    epilog = NULL, dvname = NULL, logtrans = FALSE, covplt = FALSE, 
    grp = NULL, grpnames = NULL, cont.cov = NULL, cat.cov = NULL, 
    par.list = NULL, eta.list = NULL, missing = -99, dosbox = TRUE, nochecksum = FALSE, grid = FALSE, nice=FALSE) 
{
    start <- getwd()
    Platform <- "Windows"
    if (.Platform$OS.type == "unix" & regexpr("apple", version$platform) > 
        1) {
        Platform <- "Mac"
    }
    if (.Platform$OS.type == "unix" & regexpr("bsd", version$platform) > 
        1) {
        Platform <- "Nix"
    }
    if (Platform == "Windows") {
        concurrent <- FALSE
        boot <- 0
    }
    if (Platform == "Mac" & grid==FALSE) {
        concurrent <- FALSE
        boot <- 0
    }
    
    if (Platform == "Mac" & grid==TRUE) {
        concurrent <- TRUE
        #boot <- 0
    }
    
    delim <- "/"
    testfile <- paste(ProjectDir, delim, b[1], ".ctl", sep = "")
    if (!file.exists(testfile)) 
        stop(paste("Can't find", testfile, "."))
    if (any(!file.exists(paste(ProjectDir, delim, b, ".ctl", 
        sep = "")))) 
        stop("One or more control stream(s) missing.")
    if (is.null(boot)) {
        boot <- 0
    }
    for (i in b) {
        run.args <- list(NMcom = NMcom, ProjectDir = ProjectDir, 
            i = i, boot = boot, concurrent = concurrent, Platform = Platform, 
            SGEflgs = SGEflgs, checkrunno = checkrunno, diag = diag, 
            fdata = fdata, epilog = epilog, dvname = dvname, 
            logtrans = logtrans, covplt = covplt, grp = grp, 
            grpnames = grpnames, cont.cov = cont.cov, cat.cov = cat.cov, 
            par.list = par.list, eta.list = eta.list, missing = missing, dosbox = dosbox, nochecksum = nochecksum, grid = grid, nice=nice)
        if (!concurrent) 
            do.call("runmsge", run.args)
        if (concurrent & (boot == 1 | boot == 3)) 
            do.call("runmsge", run.args)
        if (concurrent & (boot == 0 | boot == 2)) {
            pid <- fork(NULL)
            if (pid == 0) {
                do.call("runmsge", run.args)
                exit()
            }
        }
        message(paste("Run ", i, " complete.", sep = ""))
    }
    setwd(start)
    message("NONR complete.")
}

