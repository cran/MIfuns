`CWRES` <-
function(ProjectDir,b,dvname,exportas=0,logtrans=0,
           grp=NULL,grpnames=NULL,covplt,cont.cov=NULL,cat.cov=NULL,missing = -99,
           epilog=NULL){

                                        # use variables already defined in R and add additional below
                                        # Define Table File, Parameter File, and Data File name
    i<-b
    ndir<-paste(ProjectDir,"/",i,sep="")
    TabFileName<-paste(ProjectDir,"/",i,".TAB",sep="")
    ParFileName<-paste(ProjectDir,"/",i,"par.TAB",sep="")
    #DataFileName<-paste(DataDir,"/",DataFile,sep="")
                                         # get data file name from ctl stream
    ctl1<-paste(ndir,"/",i,".ctl",sep="")
    ab<-"^\\$DATA +([^ ]+).*$"
    ab1<-scan(file=ctl1,what="",comment.char="",allowEscapes=TRUE,sep="\n",quiet=TRUE)
    ab2<-grep("\\$DATA",ab1,value=TRUE)
    DataFileName<-sub(ab,"\\1",ab2)
    data.table <- read.table(TabFileName, skip = 1, header = TRUE, 
        as.is = TRUE, comment.char = "")
    runno<-i
    missing<-as.numeric(as.character(missing))

    datasetnm<-paste(ProjectDir,"/",i,"/cwtab1.deriv",sep="")
    if(file.exists(datasetnm)==TRUE){#open line 17 - check for cwres result file
     # if CWRES already exists in data.table do not recalculate it
    	if(!"CWRES" %in% names(data.table)) {
    		tab.prefix.nm <- paste(ProjectDir,"/", b, "/cwtab",sep="")
    		cwres.all<-compute.cwres(run.number=1,tab.prefix=tab.prefix.nm,printToOutfile=TRUE)
            data.cwres <- read.table(file = paste(ndir, "/cwtab1", sep = ""), skip = 1, header = TRUE)
     # read in results of cwres computation
      data.cwres<-read.table(file=paste(ndir,"/cwtab1",sep=""),skip=1,header=TRUE)
      data.table<-read.table(TabFileName,skip=1,header=TRUE,as.is=TRUE,comment.char="")
      data.table$CWRES<-data.cwres$CWRES
      write(paste("Table output from NONMEM for Run ",i," with CWRES as last column on", format(Sys.time(), "%a %b %d, %X, %Y"), sep=""),file=TabFileName,ncolumns=1)
      suppressWarnings(write.table(data.table,file=TabFileName,sep=" ",quote=FALSE,row.names=FALSE,col.names=TRUE,append=TRUE,na="."))
      
      data.cwres<-data.table
    	}
    	
    	data.cwres<-data.table    	
        data.cwres<-data.cwres[data.cwres$WRES!=0,]

     # Read Dataset, remove commented rows, and limit to one line/individual:
       setwd(ndir)
       dataset<-read.table(file=DataFileName,header=TRUE, sep=",")
      
      dataset.nC<-dataset[dataset$C != "C",]
      cov2<-dataset.nC[!duplicated(dataset.nC$ID),]
      data<-dataset.nC

      if(logtrans==1){
        data.cwres$DV<-exp(data.cwres$DV)
        data.cwres$PRED<-exp(data.cwres$PRED)
        data.cwres$IPRE<-exp(data.cwres$IPRE)}

   # add grp variable to data.cwres
   if(!is.null(grp)){
     if(is.element(grp,names(data.cwres))){
        data.cwres<-data.cwres
     }else{
          if(is.element(grp,names(cov2))){
	    data.cwres<-merge(data.cwres,cov2[ ,c("ID",grp)],by="ID")
	  }else{
            grp<-NULL
            cat("grp variable being set to NULL and excluded from plots because it does not exist in *.TAB file or in data set \n")
          }
      }
   }

      # create some graphs of cwres results
      if(exportas==0){# open line 27 - exportas==0

        if(is.null(grp)){ # open for if grp is NULL

          pdfname<-paste(ProjectDir,"/CWRESPlotReview_",i,".pdf",sep="")
          pdf(file=pdfname)

          par(mfrow = c(2, 2),
              oma = c(0, 0, 2, 0),
              mar = c(5.1, 4.5, 2.1, 2.1))

                                        # CWRES vs. PRED
          plot(x=data.cwres$PRED,y=data.cwres$CWRES,
               xlab=paste("Predicted ",dvname),
               ylab=paste("Conditional Weighted Residuals"))
                                        # Add horisontal line x=0
          abline(h=0)
                                        # Add lowess
          lines(lowess(x=data.cwres$PRED,y=data.cwres$CWRES),lty=2)

                                        # CWRES vs. TIME
          plot(x=data.cwres$TIME,y=data.cwres$CWRES,
               xlab=paste("Time (hr)"),
               ylab=paste("Conditional Weighted Residuals"))
                                        # Add horisontal line x=0
          abline(h=0)
                                        # Add lowess
          lines(lowess(x=data.cwres$TIME,y=data.cwres$CWRES),lty=2)

          if(is.element("TAD",names(data.cwres)))
            {# open for if TAD is present
                                        #setup page for graph placement and outer margin for title

                                        # CWRES vs. TAD
              plot(x=data.cwres$TAD,y=data.cwres$CWRES,
                   xlab="Time After Dose (hr)",
                   ylab="Conditional Weighted Residuals")
                                        # Add horisontal line x=0
              abline(h=0)
                                        # Add lowess
              lines(lowess(x=data.cwres$TAD,y=data.cwres$CWRES),lty=2)
            } # close for if TAD present

                                        # setup page for hist and qq plot of WRES and CWRES

          par(mfrow = c(2, 2),
              oma = c(0, 0, 2, 0),
              mar = c(5.1, 4.5, 2.1, 2.1))

          qqnorm(data.cwres$CWRES,main="Normal Q-Q Plot of CWRES")
          abline(0,1)   
                          
          qqnorm(data.cwres$WRES,main="Normal Q-Q Plot of WRES")
          abline(0,1)
           
          boxplot(data.cwres$WRES, data.cwres$CWRES, names = c("WRES", 
                                           "CWRES"), main = "Boxplots of (C)WRES")          

          #hist(data.cwres$CWRES,breaks=25,main="Histogram of CWRES", xlab="Conditional Weighted Residuals") 
          #hist(data.cwres$WRES,breaks=25,main="Histogram of WRES", xlab="Weighted Residuals") 

        }else # close for if grp is null

        { #open for if grp !is.null

          grps<-sort(unique(data.cwres[[grp]]))
          

          if(is.null(grpnames))grpnames<-grps
          else grpnames<-grpnames

          if(length(grps)!=length(grpnames)){
            grpnames<-grps
            cat(paste("Number of grouping variables does not equal number of grouping names for Run ",runno,"\n",sep=""))
	  }

                                        # create diagnostic plots with grp variable
          pdfname<-paste(ProjectDir,"/CWRESPlotReview_",grp,"_",i,".pdf",sep="")
          pdf(file=pdfname)

          j<-0
          for (j in 1:length(grps)){ # open for graphing diagnostics by grp variables

            par(mfrow = c(2, 2),
                oma = c(0, 0, 2, 0),
                mar = c(5.1, 4.5, 2.1, 2.1))

            datat<-data.cwres[data.cwres[[grp]]==grps[j],]
            xlim <-c(min(0,datat$PRED),max(datat$PRED))

                                        # CWRES vs. PRED
            plot(x=datat$PRED,y=datat$CWRES,
                 xlab=paste("Predicted ",dvname,"[",grps[j],"]"),
                 ylab=paste("Conditional Weighted Residuals"))
                                        # Add horisontal line x=0
            abline(h=0)
                                        # Add lowess
            lines(lowess(x=datat$PRED,y=datat$CWRES),lty=2)

                                        # CWRES vs. TIME
            plot(x=datat$TIME,y=datat$CWRES,
                 xlab=paste("Time (hr)"),
                 ylab=paste("Conditional Weighted Residuals"))
                                        # Add horisontal line x=0
            abline(h=0)
                                        # Add lowess
            lines(lowess(x=datat$TIME,y=datat$CWRES),lty=2)

            if(is.element("TAD",names(datat)))
              {# open for if TAD is present
                                        #setup page for graph placement and outer margin for title

                                        # CWRES vs. TAD
                plot(x=datat$TAD,y=datat$CWRES,
                     xlab="Time After Dose (hr)",
                     ylab="Conditional Weighted Residuals")
                                        # Add horisontal line x=0
                abline(h=0)
                                        # Add lowess
                lines(lowess(x=datat$TAD,y=datat$CWRES),lty=2)
              } # close for if TAD present

                                        #Overall title for plot page
            mtext(paste("Model ",i," Group = ",grpnames[j]," [",grps[j],"]"), line = 0.5, outer = TRUE)


                                        # setup page for hist and qq plot of WRES and CWRES

            par(mfrow = c(2, 2),
                oma = c(0, 0, 2, 0),
                mar = c(5.1, 4.5, 2.1, 2.1))

            qqnorm(datat$CWRES,main="Normal Q-Q Plot of CWRES")
            abline(0,1)
            
            qqnorm(datat$WRES,main="Normal Q-Q Plot of WRES")
            abline(0,1)
            
            #hist(datat$CWRES,breaks=25,main="Histogram of CWRES", xlab="Conditional Weighted Residuals") 
            #hist(datat$WRES,breaks=25,main="Histogram of WRES", xlab="Weighted Residuals")
            boxplot(datat$WRES, datat$CWRES, names = c("WRES", 
                                           "CWRES"), main = "Boxplots of (C)WRES") 

                                        #Overall title for plot page
            mtext(paste("Model ",i," Group = ",grpnames[j]," [",grps[j],"]"), line = 0.5, outer = TRUE)


          } # close for (j in 1:grps)

        } # close for if grp !is.null

        if(covplt==1){#open for covplt=1

                                        # merge cov2 (covariates) and data.cwres to get covariates into data.cwres
          all.cov<-c(cont.cov,cat.cov)
          c.names<-names(data.cwres)
          all.cov.miss<-setdiff(all.cov,c.names)
          all.cov.miss<-c("ID",all.cov.miss)
                                        #write.table(data.cwres,file=paste(ProjectDir,"/cwrestable.csv",sep=""),sep=",")
          a<-dim(data.cwres)[1]
          data.cwres<-merge(data.cwres,cov2[ ,all.cov.miss],by="ID")
          b<-dim(data.cwres)[1]
          if(all(a==b)=="FALSE") cat(paste("Problem with merge of CWRES and covariates check plots. \n"))

                                        # convert all cont.cov to numeric if they are not
          y <- data.cwres
          for (j in 1:length(cont.cov)){
            y[[cont.cov[j]]]<-as.numeric(as.character(y[[cont.cov[j]]]))
          }
          data.cwres <- y

          
          eta.list<-c("CWRES")
          eta.name<-c("Conditional Weighted Residuals")
                                        #cat.cov<-c("RACE","SEX","SMK","CYFH","CYFD","CYTH","CYTD")
                                        #cont.cov<-c("WT","BMI","LBM","AGE","CRCL","ALB")


                                        # CWRES vs Categorical Covariates
          par(mfrow=c(2,2),
              oma = c(0, 0, 0, 0),
              mar = c(4.1, 4.1, 1.5, 0.8),
              cex.axis = 0.8)

          if(!is.null(cat.cov)){
            for(p in 1:length(eta.list)) {
              for(j in 1:length(cat.cov)) {
                plot(x=as.factor(data.cwres[,cat.cov[j] ]),y=as.double(data.cwres[,eta.list[p]]),
                     xlab=cat.cov[j],ylab=eta.name[p],col="lightgrey")
                                        # Add horisontal line x=0
                abline(h=0)
              }
            }
          }


                                        # CWRES vs Continuous Covariates
                                        #par(mfrow=c(3,3),
                                        #    oma = c(0, 0, 0, 0),
                                        #    mar = c(4.1, 4.1, 1.5, 0.8),
                                        #    cex.axis = 0.8)
          
          if(!is.null(cont.cov)){   
            for(p in 1:length(eta.list)) {
              for(j in 1:length(cont.cov)) {
                x<-data.cwres
                x<-x[x[[cont.cov[j]]]!=missing,]
                cov.datar<-x
                plot(x=as.double(cov.datar[,cont.cov[j]]),y=as.double(cov.datar[,eta.list[p]]),
                     xlab=cont.cov[j],ylab=eta.name[p])
                lines(lowess(x=as.double(cov.datar[,cont.cov[j]]),y=as.double(cov.datar[,eta.list[p]])))
                                        # Add horizontal line x=0
                abline(h=0, lty=2)
              }
            }
          }
        } # close for covplt=1

        graphics.off() # end of pdf plots for exportas==0

      }else # close line   for export==0

      { # open line for exportas!=0
        cat(paste("Can't do png's yet for CWRES plots \n"))

      } # close line for exportas!=0

    }else #close check for cwres result file

    {#open - not find cwres result file
      cat(paste("Expected file cwtab1.deriv not found in NONMEM run directory.\n",sep=""))
    } # close - not find cwres result file

  } # function close

