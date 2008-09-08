`as.miTime.default` <-
function(x,format="%H:%M",...){
Sys.setenv(TZ = "GMT")
x <- as.character(x)#coerce to character
tm <- strptime(x,format=format)#convert to time object
hr <- as.numeric(strftime(tm,format="%H"))#extract character hours and coerce to number
mn <- as.numeric(strftime(tm,format="%M"))#extract character mins and coerce to number
hr <- hr + mn/60#sum hours with mins as part of hour
dy <- hr/24#convert to days
class(dy) <- c("miTime")#add class name.
return(dy)
}

