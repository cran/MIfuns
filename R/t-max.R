`tmax` <-
function(cl,v,ka,tau,...){
	ke <- ke(cl,v)
	log(
		(
			ka*
			(1-exp(-ke*tau))
		)/
		(
			ke*
			(1-exp(-ka*tau))
		)
	)/
	(ka-ke)
}

