#==========================================================================
# Utility of Choice problem 
#
#==========================================================================


# Calculate Utility
utility = function(sim,w1,w2,w3){
  sim[, u1 := as.numeric(( sum_belief > 0.5 & prhv_rsft < 0.5 ) | (sum_belief < 0.5 & prhv_rsft > 0.5))]
  sim[, u3 := 2*abs(0.5 - prhv_rsft)]
  agg = sim[, .(u1 = mean(u1), u2 = abs(mean(sum_belief) - mean(prhv_rsft)), u3 = mean(u3)), by = c("id", "samplinggroup", "trial", "state")]
  agg[, utility := c(w1,w2,w3) %*% c(u1,u2,u3)*(u1 > 0), by = c("id", "samplinggroup", "trial", "state")][order(-utility)]
  return(agg)
}


# Sampling
utility_sampling = function(sim,w1,w2,w3){
  sim = dcast(sim[,ss := NULL], ...~samplinggroup , value.var = c("sum_belief"))
  agg = sim[, .(u1 = abs(mean(3) - mean(5))), by = c("id", "trial", "state")]
  agg[, utility := c(w1,w2,w3) %*% c(u1,u2,u3)*(u1 > 0), by = c("id", "samplinggroup", "trial", "state")][order(-utility)]
  return(agg)
}

# utility <- function(x,y){
#   diff <- abs(x - y)
#   dis <-  as.numeric(( x > 0.5 & y < 0.5 ) | (x < 0.5 & y > 0.5)) # 1 if true , 0 if false
#   return( c(diff * dis))
# }
# 
# utility_of_design <- function(x, w, m1, m2){
#   colm1 <- grep(m1,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen rsft
#   colm2 <- grep(m2,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen dcpt
#   utvec <- utility(x = x[,..colm1], y = x[,..colm2]) # .. --> Ausserhalb der data table suchen
#   return(utvec)
# }
# 
# sim[ , ut := utility_of_design(.SD, w = 0.5, m1 = "sum_belief", m2 = "prhv_rsft"), by= "nr"]
# sim[ , .(utility = 1- mean((1 - ut)^2)) , by = c("id", "samplinggroup", "trial", "state")]

