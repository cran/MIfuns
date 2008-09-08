`TruncateParametersForSimulation` <-
function(parameters, bounds) {	
   nsim  <- dim(parameters)[1]
   npar  <- dim(parameters)[2]
   n1  <- range(bounds[,1])[1]
   n2  <- range(bounds[,1])[2]
   message <- ""
   if(any(round(bounds[,1]) != bounds[,1])) message <- "is not an integer"
   if(n1 < 1) message <- "is not positive"
   if(npar < n2 ) message <- "exceeds number of parameters"
   if(message != "")
  {
        print(paste("Error:","Some parameter number in bounds data frame",message),
              quote = FALSE)
        return(NULL)
   }
   for(i in 1:length(bounds[,1]) ) {
         parameters <- parameters[parameters[,bounds[i,1]] > bounds[i,2] & 
                                  parameters[,bounds[i,1]] < bounds[i,3],]
   }
   print(paste("Initial parameter set contained",nsim,"records"),quote = FALSE)
   print(paste(dim(parameters)[1],"records satisfied all bounds"),quote = FALSE)
   return(parameters)
}

