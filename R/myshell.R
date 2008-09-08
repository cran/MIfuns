`myshell` <-
function(cmd){
# runs DOS command "cmd" without opening a command window
	system(paste(Sys.getenv("COMSPEC"),"/c",cmd), intern = FALSE, wait = TRUE, input = NULL,
       show.output.on.console = FALSE,
       minimized = FALSE, invisible = TRUE)
}

