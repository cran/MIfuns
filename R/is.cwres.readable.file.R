`is.cwres.readable.file` <-
function (filename) 
{
    if (is.null(version$language)) {
        if (platform() == "WIN386") {
            access(filename, 4) == 0
        }
        else {
            filename <- paste("'", filename, "'", sep = "")
            sapply(paste("test -f", filename, "-a -r", filename), 
                unix, output = FALSE) == 0
        }
    }
    else {
        return(file.exists(filename)[1])
    }
}

