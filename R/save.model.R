`save.model` <-
function(bugs.output,model.name,parent.dir="."){

# assumes model script and R script are both named "model.name" with the extensions .txt and .R, respectively

 	myshell(paste("mkdir \"",parent.dir,"\\",model.name,"\"",sep=""))
	files = eval(parse(text=paste("c(\"codaIndex.txt\",",paste("\"coda",1:n.chains,".txt\",",
		sep="",collapse=""),paste("\"inits",1:n.chains,".txt\",",sep="",
		collapse=""),"\"data.txt\",\"log.odc\")",sep="")))
	sapply(files,function(x) myshell(paste("move ",x," \"",parent.dir,"\\",model.name,"\"",sep="")))
	myshell(paste("copy /Y \"",parent.dir,"\\",model.name,".txt\" \"",parent.dir,"\\",model.name,"\"",sep=""))
	myshell(paste("copy /Y \"",parent.dir,"\\",model.name,".R\" \"",parent.dir,"\\",model.name,"\"",sep=""))
	bugs.fit = bugs.output # looks silly but it works and appears to be necessary
	save(bugs.fit,file=paste(parent.dir,"\\",model.name,"\\",model.name,".fit.Rsave",sep=""))

}

