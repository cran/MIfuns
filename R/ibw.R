`ibw` <-
function(ht,male,floor=FALSE){
#http://www.halls.md/ideal-weight/devine.htm
#men: Ideal Body Weight (in kilograms) = 50 + 2.3 kg per inch over 5 feet.
#women: Ideal Body Weight (in kilograms) = 45.5 + 2.3 kg per inch over 5 feet.
inches <- ht/2.54
if(floor) inches[inches < 60] <- 60
over <- inches - 60
intercept <- ifelse(male,50,45.5)
round(intercept + 2.3*over,1)
}

