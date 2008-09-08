`format.miDateTime` <-
function(x,...){
x <- as.numeric(x)#coerce to numeric
dt <- trunc(x)#capture the date part
tm <- x %% 1#capture the time part
paste(format(as.miDate(dt)),format(as.miTime(tm)))
}

