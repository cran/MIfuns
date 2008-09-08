`ComputePredictiveCheck` <-
function(NonmemData,SimulatedData,
                 SimpleFun =c("min","max","mean","median"),
                 CombFun = c("Tmin","Tmax","AUC"),
                 StatFun = c("min","max","mean","median","Q25","Q75"),
                 IDname="ID",TimeName="TIME",DVname="DV",CName="C",Ignore="C") {
    if(!is.element(IDname,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",IDname,"column"))
    if(!is.element(DVname,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",DVname,"column"))
    if(!is.element(TimeName,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",TimeName,"column"))
    if(!is.element(CName,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",CName,"column"))
    if(!is.element("MDV",names(NonmemData))) stop(paste(
                                    "NonmemData does not contain","MDV","column"))

    if(IDname != "ID") NonmemData$ID <- as.character(NonmemData[,IDname])
    if(DVname != "DV") NonmemData$DV <- NonmemData[,DVname]
    if(TimeName != "TIME") NonmemData$TIME <- NonmemData[,TimeName]
    if(CName != "C") NonmemData$C <- as.character(NonmemData[,CName])
    NonmemDataSet <- NonmemData[NonmemData$C != Ignore,]
    NonmemData$STUDY <- rep(0,length(NonmemData$ID))

    if(!is.element(DVname,names(SimulatedData))) stop(paste(
                                       "SimulatedData does not contain",DVname,"column"))
    if(DVname != "DV") SimulatedData$DV <- SimulatedData[,DVname]

    nrec <- length(NonmemData$ID)
    nsimrec <- length(SimulatedData$DV)
    nsim <- nsimrec/nrec
    if(nsim != round(nsim)) stop("Error: computed number of simulations is not an integer")

    message("Simulated data set contains data for ",nsim," simulated trials")
    message("The following individual parameters will be computed for each patient:")
    message(paste(c(SimpleFun,CombFun),collapse=" "))

    message("The following statistics will be computed for each study:")
    message(paste(StatFun,collapse=" "))

    message("Computation may take a long time; please, be patient")

    SimulatedData$ID <- as.character(rep(NonmemData$ID,times=nsim))
    SimulatedData$TIME <- rep(NonmemData$TIME,times=nsim)
    SimulatedData$STUDY <- rep(1:nsim,each=nrec)
    SimulatedData$ID <- paste(SimulatedData$STUDY,SimulatedData$ID,sep="-")
 

   NonmemData    <- NonmemData[NonmemData$MDV == 0,]
   SimulatedData <- SimulatedData[NonmemData$MDV == 0,]
   CombinedData <- rbind(NonmemData[,c("ID","STUDY","DV","TIME")],
                      SimulatedData[,c("ID","STUDY","DV","TIME")])
# compute PK parameters:
    Combined <- CombinedData[!duplicated(CombinedData$ID),c("ID","STUDY")]

    if(length(SimpleFun) > 0) for(i in 1:length(SimpleFun) ) {
       ftemp <- function(x){do.call(SimpleFun[i],list(x))}
 
       temp <- aggregate.data.frame(CombinedData[,"DV"], 
                                    by=CombinedData[,"ID",drop=FALSE],FUN=ftemp)
       names(temp) <- c("ID",SimpleFun[i])
       temp$ID <- as.character(temp$ID)
       Combined <- merge(Combined,temp)
       message("Individual parameter ", SimpleFun[i]," computed")

   }
    if(length(CombFun) > 0)for(i in 1:length(CombFun) ) {
       ftemp <- function(x){do.call(CombFun[i],list(data=x))}
 
       temp <- ftemp(CombinedData)
       names(temp) <- c("ID",CombFun[i])
       temp$ID <- as.character(temp$ID)
       Combined <- merge(Combined,temp)
       message("Individual parameter ", CombFun[i]," computed")
  }
 
   Stats <- list()
   for(i in 1:length(StatFun)) {
      ftemp <- function(x){do.call(StatFun[i],list(x))}
      Stats[[i]] <- aggregate.data.frame(Combined[,c(SimpleFun,CombFun)],
                                     by=Combined[,"STUDY",drop=FALSE],FUN=ftemp)
      names(Stats[[i]]) <- c("STUDY",SimpleFun,CombFun)
      message("Summary statistic ", StatFun[i]," computed")
   } 
   Combined <- Combined[order(Combined$STUDY,Combined$ID),]
   names(Stats) <-  StatFun
   message("Output is the list with two elements: Combined and Stats")
   message("Combined is the data frame that contains individual parameters")
   message("Stats is the list, a collection of data frames, one per statistics requested")
   message("Each data frame contains summaries of individual  parameters by  study")
   message("Study 0 refers to the observed data")

   return(list(Combined=Combined,Stats=Stats))                          
}

