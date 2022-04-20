#=========================================================================
# Selection based on RSFT
#
#=========================================================================


preselect = function(data,maxdifficulty = 1, mindifficulty = 0, prbadrows) {
  
  rsft = lapply(1:nrow(data), function(i){
    d = data[i,]
    # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$b,d$ntrials,d$start)
    print(i)
    return(data.table(
      policyHV = m@compact$policyHV,
      policyLV = m@compact$policyLV,
      state = m@compact$state,
      trial = m@compact$trial,
      id = d$id
    ))
  }
  )
  
  rsft = rbindlist(rsft)
  data = merge(data,rsft, by = "id")
  
  # Difficulty -------------------------------------------------------------------------------
  d <- data[trial == 1, .(dh = policyHV, dl=policyLV), by = c("id","b")]
  data <- merge(data,d, by=c("id","b"))
  data[,difficulty := ifelse(dh >= dl,dh,dl)]
  data[, c("dh","dl") := NULL] 
  
  int <- 0.1
  boundaries <- seq(0,1, int)
  data[, dhbin := cut(difficulty, c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 1), include.lowest = T)]
  
  data <- data[difficulty <= maxdifficulty & difficulty >= mindifficulty]
  
  
  
  # bad rows ----------------------------------------------------------------------------------
  data[,bad := ifelse(round(policyHV,2) == round(policyLV,2),1,0)]
  data[,badrows := round(sum(bad)/sum(.N),2), by=c("id","b")]
  data <- data[badrows < prbadrows] # Exclusion if %bad rows > 60%
  data <- data[trial == 1,]

  
  drops <- c("policyHV", "policyLV","state","trial","bad")
  data[, c(drops) := NULL]
  
  data[, id := 1:.N]
  
  
return(data)
}






# Apply exclusion criteria before computation of the optimal model (reduce run time)
# Diff in probabilities = Diff  Risky > Diff Safe
# budget_diff[,diffh := abs(pxh-pyh)]
# budget_diff[,diffl := abs(pxl-pyl)]
# budget_diff[,difftot := round(abs(diffh-diffl),2)]
# budget_diff = budget_diff[diffh > diffl][order(-difftot)]
# budget_diff = budget_diff[difftot > 0.2]
# Delete if budget = bmix & budget = bmax

