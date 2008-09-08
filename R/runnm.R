`runnm` <-
function (NMcom, i, boot, concurrent, Platform, SGEflgs, dosbox, nochecksum, grid) 
{
  NMloc <- gsub(".pl?", "", NMcom)
  nmhome <- "/sawmill/comp/NONMEM"
  lim <- "/"
  lead <- "qsub -V -j y"
  que <- "-q all.q"
  run <- paste("-N Run", i, sep = "")
  sync <- "-sync y"
  shelby <- "-shell n -b y"
  cwd <- paste("-cwd ", nmhome, lim, NMloc, lim, "test", lim, 
               NMcom, sep = "")
  files <- paste(i, ".ctl ", i, ".lst", sep = "")
  end <- ""
  if (concurrent & (boot == 1 | boot == 2)) 
    que <- "-q bootstrap.q"
  if (concurrent & (boot == 1 | boot == 3)) 
    sync <- ""
  if (concurrent & (boot == 1 | boot == 3)) 
    end <- "&"
  nm1 <- paste(lead, que, SGEflgs, run, sync, shelby, cwd, 
               files, end)
               
  if (Platform == "Mac" & grid==FALSE) 
    nm1 <- paste("perl -S ", NMcom, " ", i, ".ctl ", i, ".lst", 
                 sep = "")
                 
  if(Platform=="Mac" & grid==TRUE){
  	NMloc <- gsub(".pl?", "", NMcom)
  nmhome <- "/common/NONMEM"
  lim <- "/"
  lead <- "qsub -V -j y"
  que <- "-q all.q"
  run <- paste("-N Run", i, sep = "")
  sync <- "-sync y"
  shelby <- "-shell n -b y"
  cwd <- paste("-cwd ", nmhome, lim, NMloc, lim, "test", lim, 
               NMcom, sep = "")
  files <- paste(i, ".ctl ", i, ".lst", sep = "")
  end <- ""
  if (concurrent & (boot == 1 | boot == 2)) 
    que <- "-q bootstrap.q"
  if (concurrent & (boot == 1 | boot == 3)) 
    sync <- ""
  if (concurrent & (boot == 1 | boot == 3)) 
    end <- "&"
  nm1 <- paste(lead, que, SGEflgs, run, sync, shelby, cwd, 
               files, end)
  }

  if (Platform == "Windows"){
  if(!nochecksum){
  if(!dosbox){
  	nm1 <- paste("cmd /C perl -S ", NMcom, " ", i, ".ctl ", 
                 i, ".lst", sep = "")
                 }else {
                 	nm1 <- paste("cmd /K perl -S ", NMcom, " ", i, ".ctl ", 
                 i, ".lst", sep = "")
                 }
                 }else
                 { 
				 
  	if(!dosbox){
  	nm1 <- paste("cmd /C perl -S ", NMcom, " ", i, ".ctl ", 
                 i, ".lst nochecksum", sep = "")
                 }else {
                 	nm1 <- paste("cmd /K perl -S ", NMcom, " ", i, ".ctl ", 
                 i, ".lst nochecksum", sep = "")
                 }
                 }
                 message(nm1)
  }
  if (Platform == "Windows") {
  	if (!dosbox){
  		system(nm1, intern = TRUE, minimized = TRUE)
  		}
  		else {
  			system(nm1, intern = FALSE, invisible = FALSE)
  			}
    }
  else {
    if (Platform == "Nix" & concurrent) {
      pid <- fork(function() system(nm1))
      wait(pid)
    }
    else {
      system(nm1)
    }
  }
}

