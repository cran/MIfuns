`as.miDateTime.default` <-
function(x,format="%m/%d/%Y %H:%M",...){
Sys.setenv(TZ = "GMT")
x <- as.character(x)#coerce to character
tm <- strptime(x,format=format)#convert to time object
cDt <- as.miDate(strftime(tm,format="%Y-%m-%d"))#extract character date and coerce to miDate
cTm <- as.miTime(strftime(tm,format="%H:%M"))#extract character time and coerce to miTime
as.miDateTime(cDt,cTm)#convert and return using as.miDateTime.miDate
}

