`AUC` <-
function(data, time="TIME", id="ID", dv="DV") {
  data <- data[order(data[[id]],-data[[time]]),]
  nrec <- length(data[[time]])
  data$diff <- c(data[[time]][-nrec] - data[[time]][-1],0)
  data$meanDV <- c((data[[dv]][-1] + data[[dv]][-nrec])/2,0)
  data$dAUC <- data$diff*data$meanDV
  data <- data[order(data[[id]],data[[time]]),]
  data <- data[duplicated(data[[id]]),]
  AUC <- aggregate.data.frame(data$dAUC,by=list(data[[id]]),FUN=sum)
  names(AUC) <- c(id,"AUC")
  return(AUC)
  }