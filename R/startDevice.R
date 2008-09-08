`startDevice` <-
function (ProjectDir, base, b) 
{
    #This function sets up a device for plotting.
    #Initialize some variables.
    oma <- c(0, 0, 2, 0)
    mar <- c(5.1, 4.5, 2.1, 2.1)
    mfrow <- c(2, 2)
    height <- 6
    width <- 6
    #Build the file name.
    file <- paste(ProjectDir, "/", base, "_", b, ".pdf", sep = "")
    #start device
    pdf(file,height=height,width=width)
    #set graphical params
    par(mfrow = mfrow, oma = oma, mar = mar)
}

