`resample.csv.filename` <-
function(x,...){
extras <- list(...)
file <- list(file=x)
valid <- c(file,extras[names(extras) %in% names(formals(read.table))])
dat <- do.call("read.csv",args=valid)
resample(dat,...)
}

