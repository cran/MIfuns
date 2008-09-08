`TABR` <-
function(ProjectDir,i,concurrent=1,boot=0,bootres=NULL,
               btcol=NULL,btcoll=NULL,btcolh=NULL,outname){
    delim <- "/"
    ndir<-paste(ProjectDir,delim,i,sep="")
    if(concurrent==1 & (boot==0|boot==2)){
	    rdir<-paste(ProjectDir,delim,i,sep="")
    }else{
	    rdir<-paste(ProjectDir,delim,i,".boot",sep="")
    }
    ctl1<-paste(rdir,delim,i,".ctl",sep="")
    
     a<-scan(file=ctl1,what="",comment.char="",allowEscapes=TRUE,sep="\n",quiet=TRUE)
#    nm.temp<-data.frame(grep('\;;(.*)',a,value=T))
    nm.temp<-data.frame(gsub('(.*);;',paste("",sep=""),grep(';;(.*)',a,value=TRUE)))
 #   nm.temp<-data.frame(gsub(';;',paste("",sep=""),grep(';;',a,value=T)))# grep variable names & remove ";;" from each name
    names(nm.temp)<-c("name")
    nm.temp1<-data.frame(strsplit(as.character(nm.temp$name),",",fixed=TRUE,extended=FALSE))
    #obviously there will be problems if someone misses a comma, because data.frame needs equal numbers of splits.
    #will the default error msg lead the scientist to suspect malformed ;; statements?
    nm.temp1<-data.frame(t(nm.temp1),row.names=NULL) 
    names(nm.temp1)<-c("NAME","TYPE","ORDER")
    # read NonmemRunLog.csv, format, assign names, and output as csv text file
    RunLogName<-paste(ProjectDir,delim,i,delim,"NonmemRunLog.csv",sep="")
    temp<-read.table(file=RunLogName,sep=",",comment.char="")
    temp<-temp[,c(6:dim(temp)[[2]])] # this line may be necessary if ..runlog.for creates large log file
#   temp<-temp[,c(6:ncol(temp)] # easier to read
    
    #if(nom>1){
    #col.tot<-nth+sum(1:nom)+nsi+5}else{
    #col.tot<-nth+nom+nsi+5}
    #temp<-temp[,c(6:col.tot)]

    tempf <- data.frame(t(temp))

    names(tempf) <- c("Estimate","RSE")
    tempf$Estimate<-as.numeric(as.character(tempf$Estimate))
    tempf$RSE<-as.numeric(as.character(tempf$RSE))
    tempf<-tempf[tempf$Estimate!=0&!is.na(tempf$Estimate)&tempf$RSE>0,]
    tempf<-cbind(tempf[1:dim(nm.temp1)[1],],nm.temp1)
    b<-c("OA","OE","SA","SE")
    z<-c("OC")
    tempf$ISV<-ifelse(
    	is.element(
		substr(
			as.character(tempf$TYPE),
			1,
			2
		),
		b
	),
	tempf$Estimate,
	as.double("NA")
    )
    tempf$temp<-substr(as.character(tempf$TYPE),1,2)
    # ISV calculation
    tempf$ISV.EST <- ifelse(!is.na(tempf$ISV)&(tempf$temp=="OA"|tempf$temp=="SA"),tempf$ISV^0.5,
                       ifelse(!is.na(tempf$ISV)&(tempf$temp=="OE"|tempf$temp=="SE"),100*(tempf$ISV^0.5),tempf$ISV))
    tempf$temp<-NULL
    # For correlated ETA's
    tempf$COV<-ifelse(is.element(substr(as.character(tempf$TYPE),1,2),z),tempf$Estimate,as.double("NA"))

    tempf$ind<-substring(as.character(tempf$TYPE),3,5)
    for (k in 1:length(tempf$Estimate)){
      if(!is.na(tempf$COV[k])){
        tempf$COR[k]<-tempf$COV[k]/(
		 ((tempf$ISV[tempf$ind==noquote(sub("([0-9]).(.*)","\\1",tempf$ind[k]))])**0.5)
		*((tempf$ISV[tempf$ind==noquote(sub("([0-9]).(.*)","\\2",tempf$ind[k]))])**0.5))
      }else{
      	tempf$COR[k]<-as.double("NA")
      }
    }
    tempf$Estimate<-as.double(signif(tempf$Estimate,3))
    tempf$RSE <- abs(round(tempf$RSE,0))# why rounding to zero places.  Are these values generally greater than zero?
    tempf$ISV <- as.double(signif(tempf$ISV,3))
    tempf$ISV.EST <- as.double(signif(tempf$ISV.EST,3))
    tempf$COR <- as.double(round(tempf$COR,3))
    tempf$TYPE<-substring(tempf$TYPE,1,2)

    for (j in 1:length(tempf$Estimate)){
        if(tempf$TYPE[j]=="T"){
            tempf$ParRSE[j]<-paste(c(tempf$Estimate[j])," (",c(tempf$RSE[j]),"%)",sep="")
          }
        if(tempf$TYPE[j]=="TC"){
            tempf$ParRSE[j]<-paste(c(tempf$Estimate[j])," (",c(tempf$RSE[j]),"%)",sep="")
          }
        if(tempf$TYPE[j]=="OE"|tempf$TYPE[j]=="SE"){
            tempf$ParRSE[j]<-paste(c(tempf$Estimate[j])," (",c(tempf$RSE[j]),"%) CV%=",c(tempf$ISV.EST[j]),sep="")
          }
       
        if(tempf$TYPE[j]=="OA"|tempf$TYPE[j]=="SA"){
            tempf$ParRSE[j]<-paste(c(tempf$Estimate[j])," (",c(tempf$RSE[j]),"%) SD=",c(tempf$ISV.EST[j]),sep="")
          }
       
        if(tempf$TYPE[j]=="OC"){
           tempf$ParRSE[j]<-paste(c(tempf$Estimate[j])," (",c(tempf$RSE[j]),"%) r=",c(tempf$COR[j]),sep="")
          }
  }
   

    # read in BootStrap results to get CI's
    tempf$ORDER<-as.numeric(as.character(tempf$ORDER))
    if(!is.null(bootres)){
        FinalBootResName <- bootres
	btres<-read.csv(FinalBootResName,sep=",",header=TRUE,na.strings=".",skip=0,comment.char = "")[,btcol]
	btres<-signif(btres[,2:4],3)
        # add CI's to tempf using columns in btres
	tempf$CI<-paste(c(btres[[btcoll]]),";",c(btres[[btcolh]]),sep="")
    	tempfinal<-tempf[,c("NAME","ParRSE","CI","ORDER")]
    }else{
        tempfinal<-tempf[,c("NAME","ParRSE","ORDER")]
    }
    tempfinal<-tempfinal[order(tempfinal$ORDER),]
    tempfinal$ORDER<-NULL
    
    write.table(tempfinal,row.names=FALSE, col.names=TRUE, sep=",", na= "." , quote=FALSE,
     file=paste(ProjectDir,outname,i,"_SumTable.txt",sep=""))
  }

