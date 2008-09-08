`Tmax` <-
function(data, id="ID", dv="DV", time="TIME") {
  ymax <- aggregate.data.frame(data[[dv]],by=list(data[[id]]),FUN=max)
  names(ymax) <- c(id,"DVmax")
  data <- merge(data,ymax)
  Tmax <- data[data[[dv]] == data$DVmax,]
  Tmax <- Tmax[!duplicated(Tmax[[id]]),c(id, time)]
  names(Tmax) <- c(id,"Tmax")
  return(Tmax)
}

