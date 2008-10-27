.First.lib <-function(lib,pkg)
{
ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
     ver <- as.character(ver)	


cat("MIfuns", ver, "loaded\n")
z<-regexpr("bsd",version$platform)
z1<-regexpr("apple",version$platform)
if(z>1|z1>1){
library(fork)
require(XML)
handleSIGCLD()
}
library(MASS) 
}
