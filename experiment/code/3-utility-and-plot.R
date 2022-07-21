#==========================================================================
# Calculate utility of each choice problem and plot choice problem with
# the highest utility
#==========================================================================

# Packages ================================================================
pacman::p_load(data.table, ggplot2, tidybayes)
memory.limit(size = 99999999)



# load data ===============================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions/function-utility.R")
source("functions/function-make-plot.R")
source("functions/function-stimuli-type.R")
source("../../analyses/models/rsft1988.R")
sim <- readRDS("../stimuli/list-stimuli-t3-predictions.rds")
stimuli <- fread("../stimuli/list-stimuli-t3.csv")
sim <- as.data.table(sim)


# Parameters ===============================================================
ntrials = 3 #total trials
weights = c(0,1,0) # c(w1,w2,w3), w1 = utility direction, w2 = utility difference, w3 = utility distance to 0.5
sampling = 3 # we calculate the utilities only for the small sampling group (3 samples)
revalue = 0.25 # probabilities smaller than 0.25 are rare events

# type
t = c(2,3) # factor trial (2 vs. 3)
re = c("re","nore") # factor (rare event vs. no rare event)
direction = c("risky","safe") # factor rsft prediction (risky vs. safe)
pars = as.data.table(expand.grid(re = re, direction = direction, t = t)) # all combinations

# Further restrictions
eout = "no" # We don't want identical outcomes for the risky option and the safe option
eprobs = "yes" # Identical probabilities for risky and safe are allowed

# Add variables ==============================================================
sim[,rare_event := ifelse(pxh <= revalue | pyh <= revalue | pxl <= revalue | pyl <= revalue, 1, 0)] # 1 = rare event, 0 = no rare event
sim[,direction := ifelse(prhv_rsft > 0.5, 1, 0)] # 1 = risky, 0 = safe
sim[,direction := ifelse(prhv_rsft < 0.5, -1, direction)]
sim[,equal_outcomes := as.numeric(pmin(xh,yh) == pmin(xl,yl) | pmax(xh,yh) == pmax(xl,yl))] # 1 = equal 0 = not equal
sim[,equal_probabilities := as.numeric(pmin(pxh,pyh) == pmin(pxl,pyl))] # 1 = equal, 0 = not equal

sim[,usafe := ifelse(state + trial*xl >= b & state + trial*yl >= b, 1, 0)]
sim[,urisky := ifelse(state + trial*xh >= b & state + trial*yh >= b, 1, 0)]
sim[,exp := urisky - usafe]


# Stimuli ==================================================================
criticalstimuli = function(pars, data, stimuli){
  
  critical = lapply(1:nrow(pars), function(i) {
    print(i)
    re = pars[i, re]
    direction = pars[i, direction]
    t = pars[i, t]
    
    # Subset by type
    subsim = type(
      data = data,
      re = re,
      t = t,
      direction = direction,
      equaloutcomes = eout,
      equalprobabilities = eprobs
    )
    
    
    # Utilities
    uoftasktot = utility(subsim,
                         w1 = weights[1],
                         w2 = weights[2],
                         w3 = weights[3])
    uoftask = uoftasktot[samplinggroup == sampling, ] # filter sampling group
    ids = uoftask[rank(-utility, ties.method = "random") %in% c(1)] # select best
    
    # Add advantage and type variables
    d = merge(stimuli[id %in% ids$id], ids[, id := as.numeric(id)], by = c("id"))
    
    m <- rsftModel(d$xh, d$yh, d$xl, d$yl, d$pxh, d$pyh, d$pxl, d$pyl, d$b,
                   ntrials, d$start, gstates = d$state, gtrials = d$trial)
    
    d[, ':='  (
      ERrisky = m@compact$policyHV,
      ERsafe = m@compact$policyLV,
      adrisky = m@compact$policyHV - m@compact$policyLV,
      type = paste(t, direction, re)
    )]
    
    
    d[, c("samplinggroup",
          "difficulty",
          "dhbin",
          "badrows",
          "idh",
          "idl",
          "u1",
          "u2",
          "u3") := NULL]
    
    
    return(data.table(d))
    }
  )
  critical = rbindlist(critical)
  return(critical)
}

d = criticalstimuli(pars,sim,stimuli)


# Controlstimuli =================================================================
cpars = data.table(re =c("re","NA","NA"), t = c(2,2,3), direction = c("safe","risky","NA"))

controlstimuli = function(pars, data, stimuli){
  control = lapply(1:nrow(pars), function(i) {
    print(i)
    re = pars[i, re]
    direction = pars[i, direction]
    t = pars[i, t]
    
    # Subset by type
    subsim = type(
      data = data,
      re = re,
      t = t,
      direction = direction,
      equaloutcomes = eout,
      equalprobabilities = eprobs
    )
    
    # 
    if(direction == "safe"){
      subsim = subsim[exp == -1,]
    } else if(direction == "risky"){
      subsim = subsim[exp == 1,]
    }

    
    # Utilities
    uoftasktot = utility_equal(subsim,
                         w1 = weights[1],
                         w2 = weights[2],
                         w3 = weights[3])
    uoftask = uoftasktot[samplinggroup == sampling, ] # filter sampling group
    ids = uoftask[rank(-utility, ties.method = "random") %in% c(1)] # select best
    
    # Add advantage and type variables
    d = merge(stimuli[id %in% ids$id], ids[, id := as.numeric(id)], by = c("id"))
    
    m <- rsftModel(d$xh, d$yh, d$xl, d$yl, d$pxh, d$pyh, d$pxl, d$pyl, d$b,
                   ntrials, d$start, gstates = d$state, gtrials = d$trial)
    
    d[, ':='  (
      ERrisky = m@compact$policyHV,
      ERsafe = m@compact$policyLV,
      adrisky = m@compact$policyHV - m@compact$policyLV
    )]
    
    dir = ifelse(d$adrisky > 0, "risky", "safe")
    raev = ifelse(any((c(d$pxh,d$pyh,d$pxl,d$pyl) < revalue) == T),"re","nore")
    d[,type := paste(t,dir, raev, "control")]
    
    d[, c("samplinggroup",
          "difficulty",
          "dhbin",
          "badrows",
          "idh",
          "idl",
          "u1",
          "u2",
          "u3") := NULL]
    
    
    return(data.table(d))
  }
  )
  
  control = rbindlist(control)
  
  return(control)
}

c = controlstimuli(cpars,sim,stimuli)
d

s = rbind(d,c)

# Make Plot =========================================================================
theplot = make_plot_critical(merge(sim[samplinggroup == "3"],unique(s[, id := as.character(id)]),by = c("id","trial","state")))
ggsave(plot = theplot, filename = paste0("../figures/Stimuli.png"), width = 5, height = 5)

#temp = s

# Add constant to outcomes and trials left to avoid outcome repetitions across stimuli ===
s[,nr := 1:.N]
s[, changed := 0]

# b = b + ntrials*val
# b = b+((ntrials+1)-trial)*val
# state = state + (trial-1)*val
addval = function(val, x, data){
  data[nr == x, ':=' (xh = xh+val, yh = yh+val, xl = xl+val, yl=yl+val, b = b+ntrials*val, state = state + (trial-1)*val, changed = changed + val)]
  return(data)
}

repeat{
  repeat {
    x = which(duplicated(s[, .(xh, yh, xl, yl)]) == T)[1]
    if(is.na(x)) {
      break
    }
      addval(1, x, s)
  }
  
  repeat {
    x = which(duplicated(s[, .(xh, yh)]) == T)[1]
    if (is.na(x)) {
      break
    }
    addval(1, x, s)
  }
  
  repeat {
    x = which(duplicated(s[, .(xl, yl)]) == T)[1]
    if (is.na(x)) {
      break
    }
    addval(1, x, s)
  }
  
  repeat {
    x = which(duplicated(s[, rbind(cbind(xh, yh), cbind(xl, yl))]) == T)[1]
    if (is.na(x)) {
      break
    }
    if (x > 11) {
      x = x / 2
    }
    addval(1, x, s)
  }
  
  if (!any(c(
    !any(!duplicated(s[, .(xh, yh, xl, yl)]) == F),
    !any(!duplicated(s[, .(xh, yh)]) == F),
    !any(!duplicated(s[, .(xl, yl)]) == F),
    !any(!duplicated(s[, rbind(cbind(xh, yh), cbind(xl, yl))]) == F)) == F
  )) {
    break
  }
}

drops = c("id","utility","changed")
s[, c(drops) := NULL]

write.table(s, "../stimuli/Stimuli_allinfos.csv",sep=";", dec=".", row.names=F)
