`format.miTime` <-
function(x,...){
Sys.setenv(TZ = "GMT")
mn <- round(as.numeric(x) * 24 * 60,0) 	#convert to total minutes
hr <- trunc(mn/60) 		#extract hours
mn <- mn %% 60			#remaining minutes
join <- sprintf("%02.0f:%02.0f",hr,mn)
#hours and mins printed as floats with 2 digits, padding with zeros as nec. (no decimal printed, sep by colon).
join[is.na(x)] <- NA#restore NA's.  'NA' looks better than "NA:NA".
join
}

