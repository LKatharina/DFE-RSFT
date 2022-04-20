#===============================================================
# function for dataset containing stimuli
#
#===============================================================

createStimuli = function(possibleoutcomes, possibleprobabilities, ntrials, start, equalev = "yes", mindiff_var, equalprob = "yes"){
  

  # all possible combinations of x and y and px
  gamble_set = expand.grid(x = possibleoutcomes[1:length(possibleoutcomes)-1],
                            px = possibleprobabilities,
                            y = possibleoutcomes)
  
  gamble_set = as.data.table(gamble_set)
  
  # calculate py, EV and Var
  gamble_set[,py := 1 - px]
  gamble_set[,ev := x * px + y * py]
  gamble_set[,var := round((x - ev)^2 * px + (y - ev)^2 * py,4)]
  
  # remove duplicates
  tab <- matrix(ncol = 2, nrow = nrow(gamble_set))
  tab <- rbind(gamble_set$ev, gamble_set$var)
  tab <- t(tab)
  dup <- duplicated(tab)
  
  gamble_set[,dup := dup]
  gamble_set <- gamble_set[dup == FALSE]
  
  #remove gambles with equal outcomes
  gamble_set <- gamble_set[x != y]
  
  # order by outcomes
  for(i in 1:nrow(gamble_set)){
    if(gamble_set$x[i] >= gamble_set$y[i]){
      gamble_set[i, ':=' (x = x, px = px, y = y, py = py)]
    } else {
      gamble_set[i,':=' (x = y,px = py, y = x, py= px)]
    }
  }
  
  # ID
  gamble_set[,id := 1:nrow(gamble_set)]
  gamble_set[,dup := NULL]
  
  
  # Stimuli --------------------------------------------------------------------
  # Pairs of gambles (risky Option vs. Safe Option) = stimuli
  # create stimuli (gamble combinations), 
  # where the two gambles have equal EVs and different variances (min difference defined above)
  
  # all possible combinations
  pairs <- matrix(ncol=2, nrow = nrow(t(combn(max(gamble_set$id),2))))
  pairs[,1:2] <- t(combn(max(gamble_set$id), 2))
  colnames(pairs) <- c("id1","id2")
  pairs <- data.table(pairs)
  
  rset1 <- gamble_set[id %in% pairs[,id1],.(id1 = id ,ev1 = ev, var1 = var, x1 = x, y1 = y,
                                            px1 = px, py1 = py)]
  rset2 <- gamble_set[id %in% pairs[,id2],.(id2 = id ,ev2 = ev, var2 = var, x2 = x, y2 = y,
                                            px2 = px, py2 = py)]
  pairs <- merge(pairs,rset1, by = "id1")
  pairs <- merge(pairs, rset2, by = "id2")
  
  # ID
  pairs[, id := 1:.N]
  
  
  # order by variance -----------------------------------------------------------
  pairs[,varh := max(var1,var2),by=id]

  pairs[varh == var1, ':=' (evh = ev1, evl = ev2, varh = var1, varl = var2, xh = x1, yh = y1 , xl = x2,
                            yl = y2, pxh = px1, pyh = py1,
                            pxl = px2, pyl = py2, idh = id1, idl = id2)]
  pairs[varh == var2, ':=' (evh = ev2, evl = ev1, varh = var2, varl = var1, xh = x2, yh = y2, xl = x1,
                            yl = y1, pxh = px2, pyh = py2,
                            pxl = px1, pyl = py1, idh = id2, idl = id1)]
  
  
  drops <- c("ev1","ev2","x1", "x2", "y1", "y2", "px1", "px2", "py1", "py2", "var1", "var2",
             "id1", "id2")
  pairs[, c(drops) := NULL]
  
  
  # Preselection -----------------------------------------------------------------
  
  # Expected Value Delete combinations with different EVs and differences in variances < mindiff_var
  if(equalev == "yes"){
    pairs <- pairs[evh == evl,]
  }
  
  # Minimum variance-difference between gambles
  pairs <- pairs[abs(varh - varl) > mindiff_var,]
  
  
  # Equal Probabilities
  if(equalprob == "no"){
    pairs$prequal = pairs[,ifelse(pxh %in% c(pxl,pyl),1,0), by = id]$V1
    pairs = pairs[prequal != 1,]
    pairs[,prequal := NULL]
  }
  
  
  # Budget -------------------------------------------------------------
  # Budget --> Amount of points you have to reach within t trials.
  
  # Range budget
  #b min = minimal outcome of the two options * t (trials)
  #b max = maximal outcome of the two options * t (trials)
  #t = 5 --> defined at the beginning
  pairs[, id := 1:.N]
  pairs[,bmin := min(xh,xl,yh,yl)*ntrials,by=id]
  pairs[,bmax := max(xh,xl,yh,yl)*ntrials,by=id]
  
  
  # Create table with rows = number of stimuli * Sum of all possible budgets
  # number of rows
  
  nor <- pairs[,.(l = length(bmin:bmax)), by = id][, sum(l)]
  rps <- pairs[,.(l = length(bmin:bmax)), by = id]
  
  repetitions = NULL
  for(i in 1:nrow(rps)){
    v = rep(rps$id[i], each = rps$l[i])
    repetitions = c(repetitions,v)
  }
  
  budgets = NULL
  for(i in 1:nrow(pairs)){
    v = pairs[i,bmin]:pairs[i,bmax]
    budgets = c(budgets,v)
  }
  
  pairs = pairs[repetitions,]
  pairs$b = budgets
  
  
  pairs[,start := 0]
  
  
  
  # delete colums -----------------------------------------------------------------------------
  drops <- c("bmin", "bmax","id")
  pairs[, c(drops) := NULL]
  pairs[, id := 1:.N]
  pairs$ntrials = ntrials
  return(pairs)
}


# budget_diff[,diffh := abs(pxh-pyh)]
# budget_diff[,diffl := abs(pxl-pyl)]
# budget_diff[,difftot := round(abs(diffh-diffl),2)]
# budget_diff = budget_diff[diffh > diffl][order(-difftot)]
# budget_diff = budget_diff[difftot > 0.2]
