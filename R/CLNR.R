`CLNR` <-
function (Dir, ProjectDir, note = "Files removed", test = TRUE) 
{
    Platform <- "Windows"
    z<-regexpr("bsd",version$platform)
    if (z>0){Platform <- "Nix"}else{
         Platform <- "Windows"}
    FilesToRemove <- dir(path = Dir, full.names = TRUE, recursive = TRUE)
    FilesToRemoveComplete <- file.info(FilesToRemove)
    if (test == FALSE) {
        print(FilesToRemove)
        answer <- readline("Are you sure you want to delete these files? ")
        if (answer == "y" | answer == "Y") {
            file.remove(FilesToRemove)
            FilesToRemove <- c(note, FilesToRemove)
            FileNm <- paste(ProjectDir, "/", "FilesRemoved_", 
                gsub("[[:space:]]|:", "_", date()), ".txt", sep = "")
            write.table(FilesToRemoveComplete, file = FileNm, 
                sep = ",", quote = FALSE, row.names = TRUE, col.names = FALSE, 
                append = FALSE)
            answer2 <- readline(paste("Do you also want ", Dir, 
                " and all it's subdirectories removed? ", sep = ""))
            if (answer2 == "y" | answer2 == "Y") {
                setwd(ProjectDir)
                if (Platform == "Windows") {
                  del.dir <- paste("cmd /C rmdir /S /Q ", "\"", 
                    Dir, "\"", sep = "")
                  system(del.dir)
                }
                else {
                  deldir <- paste("rm -rf ", Dir, sep = "")
                  system(deldir)
                }
            }
            else cat(paste("Files removed but ", Dir, " not removed", 
                "\n", sep = ""))
        }
        else cat(paste("Removal of files aborted by user", "\n", 
            sep = ""))
    }
    else {
        cat(paste("Files that would have been deleted if test=TRUE", 
            "\n", sep = ""))
        dir(path = Dir, full.names = TRUE, recursive = TRUE)
    }
}

