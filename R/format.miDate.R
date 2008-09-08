`format.miDate` <-
function(x,...){
Sys.setenv(TZ = "GMT")
format.Date(x,format="%m/%d/%Y")
}

