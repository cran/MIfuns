`Tmin` <-
function(data, id="ID", dv="DV", time="TIME") {
  ymin <- aggregate.data.frame(data[[dv]],by=list(data[[id]]),FUN=min)
  names(ymin) <- c(id,"DVmin")
  data <- merge(data,ymin)
  Tmin <- data[data[[dv]] == data$DVmin,]
  Tmin <- Tmin[!duplicated(Tmin[[id]]),c(id,time)]
  names(Tmin) <- c(id,"Tmin")
  return(Tmin)
}

