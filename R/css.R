`css` <-
function(cl,v,ka,tau,dose,time,...){
	ke <- ke(cl,v)
	dose*ka/(v*(ka-ke))*(exp(-ke*time)-exp(-ka*time))*acr(cl,v,tau)
}

