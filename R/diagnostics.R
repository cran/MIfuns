`diagnostics` <-
function (grp, grpnames, ProjectDir, b, dataObs, dvname,  covplt) 
{
  plots <-list()
  data <- dataObs
  if (is.null(grp)) {
    data$plotrGroup <- "all"
    grp <- "plotrGroup"
  }
  grp <- intersect(grp,names(data))
  data$grpnames <- factor(
  	do.call(
  		paste,
  		c(
  			as.list(data[,grp,drop=FALSE]),
  			sep=", "
  		)
  	)
  )
  nlevs <- length(levels(data$grpnames))
  if(!is.null(grpnames))if(length(grpnames)==nlevs)levels(data$grpnames) <- grpnames
  if(!is.null(grpnames))if(length(grpnames)!=nlevs)warning(
  	paste(
  		"Run", 
  		b, 
  		"has",
  		nlevs,
  		"grouping levels but",
  		length(grpnames),
  		"grpnames (ignored)." 
  	)
  )
  observed <- melt(data,measure.var=intersect(c("PRED","IPRE"),names(data)),id.var=c("DV","grpnames"))
  observed$variable <- factor(
  	observed$variable,
  	levels=intersect(c("PRED","IPRE"),names(data)),
  	labels=c("population","individual")[c("PRED","IPRE") %in% names(data)]
  )
  groupSubtitle <- function(grp){
  	if(grp[[1]] == "plotrGroup")return(NULL)
  	paste("by",paste(grp,collapse=", "))
  }
  #Observed vs. Predicted 
  plots$obsPred <- xyplot(
  	DV ~ value | grpnames + variable, 
  	observed,
  	as.table=TRUE,
  	aspect=1,
  	layout=c(2,2),
  	xlim = with(data,c(min(0, DV, PRED), max(0, DV, PRED))),
  	ylim = with(data,c(min(0, DV, PRED), max(0, DV, PRED))),
  	ylab = paste("Observed", dvname), 
  	xlab = paste("Predicted", dvname),
  	panel= function(x,y,...){
  		panel.xyplot(x,y,...)
  		panel.abline(0,1)
  	},
  	main=paste("Model",b,"\nObserved vs. Predicted",groupSubtitle(grp))
  )
  #Residuals vs. Predicted
  resvar <- intersect(c("RES","WRES","CWRES"),names(data))
  resid <- intersect(c("PRED","TIME","grpnames","TAD"),names(data))
  res <- melt(data,measure.var=resvar,id.var=resid)
  plots$resPred <- xyplot(
  	value ~ PRED | grpnames + variable, 
  	res,
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Predicted", dvname),
  	panel= function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  	},
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nResiduals vs. Predicted",groupSubtitle(grp))
  )
  #Residuals vs. Time
  plots$resTime <- xyplot(
  	value ~ TIME | grpnames + variable, 
  	res,
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Time (hr)"),
  	panel= function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  	},
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nResiduals vs. Time by",groupSubtitle(grp))
  )
  #Residuals vs. TAD
  if("TAD" %in% names(res))plots$resTad <- xyplot(
  	value ~ TAD | grpnames + variable, 
  	res,
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Time (hr)"),
  	panel= function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  	},
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nResiduals vs. TAD",groupSubtitle(grp))
  )
  #QQ-Norm
  plots$resQ <- qqmath(
  	~ value | grpnames + variable, 
  	res,
  	as.table=TRUE,
  	layout=c(2,2),
  	aspect=1,
  	ylab = "residuals", 
  	xlab = paste("theoretical quantiles"),
  	panel = function(x, ...) {
    	panel.qqmathline(x, ...)
    	panel.qqmath(x, ...)
    },
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nNormal Q-Q Plot Residuals",groupSubtitle(grp))
  )
  #QQ-Res
  if("CWRES" %in% names(data))plots$resCwresQ <- qq(
  	variable ~ value | grpnames,
  	res,
  	as.table=TRUE,
  	layout=c(2,2),
  	aspect=1,
  	subset=variable %in% c("WRES","CWRES"),
  	panel = function(...) {
    	panel.qq(...)
    	panel.abline(0,1)
    },
  	main=paste("Model",b,"\nQ-Q Plot of CWRES vs. WRES",groupSubtitle(grp))
  )
  #Residuals
  plotsRes <- bwplot(
  	value ~ variable | grpnames,
  	res,
  	main="Boxplots of Residuals",
  	ylab="residuals"
  )
  plots
}

