`PlotPredictiveCheck` <-
function(PCData,
                 Parameters=c("min","max","mean","median","Tmin","Tmax","AUC"),
                 Summaries = c("min","max","mean","median","Q25","Q75"),
                 plot.hist=TRUE,plot.qq=TRUE,DrugName = "DV",plotType="pdf",
                 FileTemplate="Plots"){
 if(plotType == "wmf") {
    plotDev <- win.metafile
    FileName <- paste(FileTemplate,"%03d.wmf",sep="")
 }
 if(plotType == "pdf") {
    plotDev <- pdf
    FileName <- paste(FileTemplate,".pdf",sep="")
 }
 if(plotType == "ps") {
    plotDev <- postscript
    FileName <- paste(FileTemplate,".ps",sep="")
 }
 if(plotType == "png") {
    plotDev <- png
    FileName <- paste(FileTemplate,"%03d.png",sep="")
 }
 if(plotType == "jpeg") {
    plotDev <- jpeg
    FileName <- paste(FileTemplate,"%03d.jpeg",sep="")
 }
 if(plotType == "bmp") {
    plotDev <- bmp
    FileName <- paste(FileTemplate,"%03d.bmp",sep="")
 }

 Combined <- PCData$Combined
 Stats <- PCData$Stats

 Parameters <- Parameters[is.element(Parameters,names(Combined[c(-1,-2)]))]
 Summaries <- Summaries[is.element(Summaries,names(Stats))]
 plotDev(FileName)
 if(plot.hist) for(ipar in Parameters) for(isum in Summaries) {
     parName  <- paste(isum,ipar,DrugName)
     Observed  <- Stats[isum][[1]][,ipar][Stats[isum][[1]][,"STUDY"] == 0]
     Simulated <- Stats[isum][[1]][,ipar][Stats[isum][[1]][,"STUDY"] != 0]
     p.est <- length(Simulated[Simulated >= Observed])/length(Simulated)
     p.est <- 2*min(p.est,1-p.est)
     if(all(Simulated == Simulated[1])) {
          plot(x=1,y=1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
          text(x=1,y=1.2,labels=paste("Parameter:",parName))
          text(x=1,y=1.0,labels=paste("All simulated values are equal to",Simulated[1]))
          text(x=1,y=0.8,labels=paste("Observed value is equal to",Observed))
     }else{
         hist(Simulated,probability=TRUE,main=paste(parName," observed = ",round(Observed,1),
               " (p = ",round(p.est,4),")",sep=""),xlab=paste("Simulated",parName))
         abline(v=Observed)
     }
   }

 if(plot.qq) for(ipar in Parameters) {
     qqi<-function(x)(qqplot(x,Combined[,ipar][Combined[,"STUDY"]==0],plot=FALSE))
     test<-tapply(Combined[,ipar], Combined[,"STUDY"], qqi)
     test.x <- NULL
     test.y <- NULL
     for(i in 2:length(test)) {
        test.x<-cbind(test.x,test[[i]]$x)
        test.y<-cbind(test.y,test[[1]]$y)
     }
  
     matplot(as.matrix(test.y),as.matrix(test.x), type="l", lty=1,
          xlab=paste("Observed",ipar),ylab=paste("Simulated", ipar)) 
     abline(0,1, lwd=2)

   }
 dev.off()
 message("Results are saved in the file(s) ",FileName)
 return(NULL)
}

