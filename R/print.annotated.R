`print.annotated` <-
function(x,...){
	print.data.frame(x)
	print(attributes(x)$annotations)
}

