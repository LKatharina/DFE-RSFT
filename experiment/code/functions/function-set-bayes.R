# ===============================================================
# set bayes function
#
# ===============================================================


bayesbeliefs = function(dt){
  dbelief = computeBeliefs(dt$ss,xh = dt$xh, dt$yh, dt$pxh ,dt$xl ,dt$yl ,dt$pxl , dt$b, ntrials = dt$ntrials,
                           start = dt$start, nbetadraw = 800, tau = 0.2, prior = c(1,1), seed = 100)
}
