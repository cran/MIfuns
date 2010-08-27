.First.lib <-function(lib,pkg)
{
    ver <- as.character(
    	read.dcf(
		file.path(
			lib, 
			pkg, 
			"DESCRIPTION"
		), 
		"Version"
	)
    )
    cat("MIfuns", ver, "loaded\n")
    if(.Platform$OS.type == 'unix'){
	    library(fork)
	    require(XML)
	    handleSIGCLD()
    }
    library(MASS) 
}
