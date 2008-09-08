`lbm` <-
function(wt,ht,male){
males <- 1.10 * wt - 128 * (wt^2/ht^2)
females <- 1.07 * wt - 148 * (wt^2/ht^2)
ifelse(male,males,females)
#http://www.halls.md/body-mass-index/leanbody.htm
#Lean Body Weight (men) = (1.10 x Weight(kg)) - 128 x ( Weight2/(100 x Height(m))2)
#Lean Body Weight (women) = (1.07 x Weight(kg)) - 148 x ( Weight2/(100 x Height(m))2)
}

