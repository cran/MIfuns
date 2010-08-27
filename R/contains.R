`contains` <-
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}

`text2decimal` <-
function(x)as.numeric(sub("[^0-9.+-]*([+|-]?[0-9]+\\.?[0-9]*).*","\\1",as.character(x)))

reapply <- 
function (x, INDEX, FUN, ...,simplify=TRUE) 
{
    form <- tapply(x, INDEX)
    calc <- tapply(x, INDEX, FUN, ...,simplify=simplify)
    need <- table(form)
    calc <- lapply(
    	seq(
    		length.out=length(calc)
    	),
    	function(cell)if(
    		!as.character(cell) %in% names(need)
    	) return(calc[[cell]]) else rep(
    		calc[[cell]],
    		length.out=need[[
    			as.character(cell)
    		]]
    	)
    )
	grps <- split(form,form)
	grps <- lapply(
		grps, 
		function(grp)seq(
			length.out=length(grp)
		)
	)
	elem <- unsplit(grps,form)
	sapply(
		seq(
			length.out=length(form)
		),
		function(i)calc[[
			form[[i]]
		]][[
			elem[[i]]
		]]
	)
}

aug <- function(x,...){
	extras <- list(...)
	nms <- names(extras)
	for(name in nms)x[[name]] <- extras[[name]]
	x
}
`stableMerge` <-
function(x,y){
#left join with stable row count, row order, column order, and row names
#returns equiv of x with values of y appended (looked up by common columns)
colnames <- names(x)
rowNames <- rownames(x)
if(all(names(y) %in% names(x))){
warning("nothing to merge")
return(x)
}
key <- names(y)[names(y) %in% names(x)]
if (
!identical(
nrow(y[,key,drop=FALSE]),
nrow(unique(y[,key,drop=FALSE]))
)
) stop("keys in y not unique")
x$stable_Index <- 1:nrow(x)
z <- merge(x,y,all.x=TRUE,all.y=FALSE,sort=FALSE)
z <- z[order(z$stable_Index),]
z$stable_Index <- NULL
z <- z[,c(colnames,names(z)[!names(z) %in% colnames])]
rownames(z) <- rowNames
return(z)
}

