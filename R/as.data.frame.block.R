`as.data.frame.block` <-
function(x,...){
	con <- textConnection(x)
	dat <- read.table(con,header=TRUE,as.is=TRUE,check.names=FALSE,...)
	close(con)
	dat
}

