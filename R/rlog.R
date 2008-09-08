`rlog` <-
function (b, boot, ProjectDir, runlog) 
{
  k <- 0
     # determine platform
     Platform <- "Windows"
     z<-regexpr("bsd",version$platform)
     if (z>0){Platform <- "Nix"}else{Platform <- "Windows"}

  for (j in b) {
    k <- k + 1
    if (Platform == "Nix") {
      if (boot == 0 | boot == 2) {
        rdir <- paste(ProjectDir, "/", j, sep = "")
        delfd <- paste("rm -rf ", rdir, "/FD*", sep = "")
        system(delfd)
      }
      else {
        rdir <- paste(ProjectDir, "/", j, ".boot", sep = "")
        testf<-paste(rdir,"/FILE10",sep="")
        if(!file.exists(testf)){
        delfd <- paste("rm -rf ", rdir, "/F*", sep = "")
        delfd2 <- paste("rm -rf ", rdir, "/nonmem.exe", 
                        sep = "")
        delfd3 <- paste("rm -rf ", rdir, "/P*", sep = "")
        delfd4 <- paste("rm -rf ", rdir, "/O*", sep = "")
        delfd5 <- paste("rm -rf ", rdir, "/Run*", sep = "")
        system(delfd)
        system(delfd2)
        system(delfd3)
        system(delfd4)
         system(delfd5)}
      }
    }
    if (Platform == "Windows") {
      rdir <- paste(ProjectDir, "/", j, sep = "")
    }
    FileName <- paste(rdir, "/NonmemRunLog.csv", sep = "")
    if (file.exists(FileName) == "TRUE") {
      temp <- read.table(FileName, sep = ",", comment.char = "", 
                         )
      temp.num <- cbind(temp, j)
      prefinal <- temp.num
      CombRunLog <- data.frame(prefinal)
      CombRunLogName <- paste(ProjectDir, "/", "CombRunLog.csv", 
                              sep = "")
      if (runlog == 1) {
        write.table(CombRunLog, file = CombRunLogName, 
                    sep = ",", quote = FALSE, row.names = FALSE, 
                    col.names = FALSE, append = TRUE, na = ".")
      }
      else {
        if (k <= 1) {
          write.table(CombRunLog, file = CombRunLogName, 
                      sep = ",", quote = FALSE, row.names = FALSE, 
                      col.names = FALSE, append = FALSE, na = ".")
        }
        else {
          write.table(CombRunLog, file = CombRunLogName, 
                      sep = ",", quote = FALSE, row.names = FALSE, 
                      col.names = FALSE, append = TRUE, na = ".")
        }
      }
    }
    else {
      cat(paste("Run Log for Run ", j, " does not exist", 
                "\n", sep = ""))
      next
    }
  }
}

