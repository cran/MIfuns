`check.data` <-
function(IDname,data){
   columnNames <- names(data)
   print(paste(length(data[,IDname]),"records from",length(unique(data[,IDname])),"patients"))
   for(columnName in columnNames) {
     if( any(is.na(data[,columnName]))) {
       temp <- data[,c(IDname,columnName)]
       names(temp) <- c("ID","value")
       nsubAll <- length(unique(temp$ID))
       nsubNA <- length(unique(temp$ID[is.na(temp$value)]))
       nsub   <- length(unique(temp$ID[!is.na(temp$value)]))
       if(nsubAll == nsub + nsubNA){
          print(paste(columnName,": ",nsubNA, " (",round(100*nsubNA/nsub,1),
             "%) of patients have only missing values",sep=""))
       }else{
        print(paste(columnName,": ",nsubAll-nsubNA," (",
                 round(100*(nsubAll-nsubNA)/nsubAll,1),"%) of patients have all data;",sep=""))
        print(paste("       ",nsubAll-nsub," (",round(100*(nsubAll-nsub)/nsubAll,1),
             "%) of patients have only missing values;",sep=""))
        print(paste("       ",nsubNA+nsub -nsubAll," (",round(100*(nsubNA+nsub -nsubAll)/nsubAll,1),
                 "%) of patients have some missing values",sep=""))
       }  
     }
   }

   for(columnName in columnNames) {
     temp <- data[,columnName]
     temp <- temp[!is.na(temp)]
     if( any(is.na(as.double(as.character(temp))))) {
       print(paste(columnName,"is not a numeric variable"))
       if(length(unique(temp)) < 10) {
          print(paste(columnName,"levels:",paste(sort(unique(temp)),collapse=",")))
        }
     }else{
        if(length(unique(temp)) < 10) {
          print(paste(columnName,"levels:",paste(sort(unique(temp)),collapse=",")))
        }else{
           print(paste(columnName,"range:",range(temp)[1],"-",range(temp)[2])) 
        }  
     }
   }

   columnNames <- columnNames[columnNames != IDname]
   for(columnName in columnNames) {
       temp <- data[,c(IDname,columnName)]
       names(temp) <- c("ID","value")
       temp <- temp[!is.na(temp$value),]
       if(length(unique(temp$ID)) != length(unique(paste(temp$ID,temp$value))) ) {
          print(paste(columnName,": vary with time within some patients"))
       } else {
                print(paste(columnName,": constant within all patients"))       
       }
   }
}

