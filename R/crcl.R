`crcl` <-
function(age,wt,male,scr){
clearance <- (140-age)*wt/(72*scr)#scr: mg/dL
clearance[!male] <- clearance[!male] * 0.85
return(clearance)
}

