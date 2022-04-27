# ===============================================================
# Function to compute Beliefs: only for one number of draws and
# task
# ===============================================================

computeBeliefs = function(ss,xh,yh,pxh,xl,yl,pxl,b,start,ntrials, nbetadraw = 800, tau = 0.2, prior = c(1,1), seed = 1000){
  set.seed = seed
  # number of possible occurrence of the outcome xh, given the number of draws ss
  ob = 0:ss
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2))) # alle Kombinationen von möglichen Obs der Optionen miteinander
  
  # Beta: Beliefs about the probabilities
  belx = vector("list", (ss+1))
  
  # Sampling from beta distribution
  for(i in 1:(ss+1)){
    belx[[i]] = rbeta(nbetadraw,ob[i] + prior[1], ss-ob[i] + prior[2])
  }
  bely = lapply(belx, function(x){ 1 - x })
  
  # data table
  data = data.table(xh = xh, yh = yh, pxh = pxh, pyh = 1 - pxh,
                    xl = xl, yl = yl, pxl = pxl, pyl = 1 - pxl,
                    bxh = unlist(belx[com[,1]]), byh = unlist(bely[com[,1]]),
                    bxl = unlist(belx[com[,2]]), byl = unlist(bely[com[,2]]),
                    start = start, b = b, id_betadraw = rep(1:nbetadraw,length(belx)), count_xh = rep(com[,1]-1,each = nbetadraw),
                    count_xl = rep(com[,2]-1,each = nbetadraw), id_binom = rep(1:nrow(com),each = nbetadraw))
  
  data[,nr := 1:.N]
  data[,c("bxh","byh","bxl","byl") := lapply(.SD,round,digits = 3), .SDcols = c("bxh","byh","bxl","byl")]
  # id_binom: id für eine mögliche Kombination aus xh und xl Ziehungen, insgesamt gibt es alle möglichen Kombinationen
  # pro id_binom (Kombination) werden dann nbetadraw Beliefs aus der Betaverteilung gezogen = id_betadraw gibt die Ziehung pro
  # Kombination (id_binom) an. nbetadraw * Kombinationen von Gambles
  
  # Objective optimal model
  rsft_dfd <- hm1988(
    ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
    trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
    data = data[1,],            # our data (as before)
    budget = ~b,       # name of our budget column in our data
    initstate = start,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = tau))
  
  rsft_dfd_sim <- data.table(
       trial = (ntrials+1) - rsft_dfd$get_timehorizons(), # get trials that are *remaining*
       state =   rsft_dfd$get_states(),         # get possible states
       prhv_rsft =    predict(rsft_dfd)  # get pr(hv) prediction
  )
  
  # Optimal model based on subjective probabilties
  rsft_dfe = lapply(1:nrow(data), function(i){
    d = data[i,]
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$bxh,d$byh,d$bxl,d$byl,d$b,ntrials,d$start)
    choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],tau))
    print(i)
    return(data.table(
      policyHV_belief = m@compact$policyHV,
      policyLV_belief = m@compact$policyLV,
      state = m@compact$state,
      trial = m@compact$trial,
      prhv_belief = choiceprob$policyHV,
      nr = i
    ))
  }
  )
  
  rsft_dfe_sim = rbindlist(rsft_dfe)
  sim = merge(data,rsft_dfe_sim, by = "nr")
  sim = merge(sim, rsft_dfd_sim, by = c("trial", "state"))
  sim = cbind(sim, rsft_dfe_sim[,.(prhv_belief,nr)]) # RSFT Lösung für Beliefs + sim zusammenfügen
  sim[nr %in% sim[,sum(.N), by = nr][V1 != 9]$nr]
  # Probability to draw xh (xl) 1...ss times given the underlying distributions and the total number of draws ss.
  # Binominal distribution provides the probabilities of obtaining xh (xl). 
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  
  # Joint Probability of obtaining xh and xl.
  sim[,pr_joint := binomh[(count_xh+1)] * binoml[(count_xl+1)]]
  
  
  sim[,wprhv_belief := prhv_belief * pr_joint] # Weighting belief with joint probability
  gamble = unique(sim[,.(trial, state, prhv_rsft)])
  agg = sim[,.(sum_belief = sum(wprhv_belief)), by = c("trial","state","id_betadraw")] # Sum 
  agg = merge(agg,gamble, by = c("trial","state"))
  agg = agg[order(id_betadraw,trial,state)]


  return(agg)
}
