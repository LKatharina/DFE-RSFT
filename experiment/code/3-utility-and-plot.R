#==========================================================================
# Calculate utility of each choice problem and plot choice problem with
# the highest utility
#==========================================================================

# Packages ----------------------------------------------------------------
pacman::p_load(data.table, ggplot2, tidybayes)
memory.limit(size = 99999999)
# load data ---------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions/function-utility.R")
source("functions/function-make-plot.R")
source("../../analyses/models/rsft1988.R")
sim <- readRDS("../stimuli/list-stimuli-t3-predictions.rds")
stimuli <- fread("../stimuli/list-stimuli-t3.csv")
sim <- as.data.table(sim)

#parameters ---------------------------------------------------------------
ntrials = 3
t = 3
sampling = 3 


# filter by trial
sim = sim[trial == t]

# Weights 
weights = as.data.table(expand.grid(w1 = seq(0,1,1/3), w2 = seq(0,1,1/3), w3 = seq(0,1,1/3)))[,Sum := rowSums(.SD)][Sum == 1]
weights[,Sum := NULL]


# Utility ------------------------------------------------------------------
for(i in 1:nrow(weights)){
  uoftasktot = utility(sim,w1 = as.numeric(weights[i,1]),w2 = as.numeric(weights[i,2]),w3 = as.numeric(weights[i,3]))
  uoftask = uoftasktot[samplinggroup == sampling,]
  weights[i, ids := list(uoftask[rank(-utility, ties.method = "random") %in% c(1:60)])]

}


# Plots ---------------------------------------------------------------------
for(i in 1:nrow(weights)){
  d = merge(sim,weights[,ids[i]][,samplinggroup := NULL],by = c("id","trial","state"))
  d = d[samplinggroup == "3"]
  plot = make_plot_critical(d)
  weights[i, plot := list(make_plot_critical(d))]
}

#weights[,plot[[4]]]


# Take a closer look at the stimuli -----------------------------------------
ids = weights$ids[[4]]
stimuli = merge(stimuli[id %in% ids$id],ids[,id := as.numeric(id)],by = c("id"))
stimuli = stimuli[order(-utility)]
stimuli[,dubl := 1:.N, by = c("idl", "idh")]
stimuli = stimuli[dubl == 1]

rsft = lapply(1:nrow(stimuli), function(i){
  d = stimuli[i,]
  m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$b,ntrials,d$start,gstates = d$state, gtrials = d$trial)
  print(i)
  return(data.table(
    state =  m@compact$state,
    trial = m@compact$trial,
    policyHV_belief = m@compact$policyHV,
    policyLV_belief = m@compact$policyLV,
    id = d$id
  ))
}
)

stimuli = merge(stimuli,rbindlist(rsft), by = c("id","state", "trial"))
stimuli[ ,adrisky := round(policyHV_belief - policyLV_belief,2)]
stimuli = stimuli[order(-utility)]
stimuli[ ,c("samplinggroup","difficulty","dhbin","badrows","dubl","policyHV_belief","policyLV_belief", "idh", "idl","u1","u2","u3") := NULL]


if(t == 2){
  best = rbind(stimuli[adrisky < 0][1:2,],stimuli[adrisky > 0][1:2,])[,criterion := "best"]
  equaldiff = rbind(stimuli[adrisky < 0][2:3,],stimuli[adrisky > 0][1:2,])[,criterion := "difficulty"]
  inequaloutcomes = rbind(stimuli[adrisky < 0][c(7,10),],stimuli[adrisky > 0][1:2,])[,criterion := "outcomes & difficulty"]
  s = rbind(best,equaldiff,inequaloutcomes)
  write.table(s, "../stimuli/stimuli-trial2.csv",sep=";", dec=".", row.names=F)
  plot = make_plot_critical(merge(sim[samplinggroup == "3"],unique(s[, id := as.character(id)]),by = c("id","trial","state")))
  ggsave("../figures/stimuli-t2.png", width = 10, height = 10, scale = 1)
}

if(t == 3){
  best = rbind(stimuli[adrisky < 0][1:2,],stimuli[adrisky > 0][1:2,])[,criterion := "best"]
  inequaloutcomes = rbind(stimuli[adrisky < 0][c(2,5),],stimuli[adrisky > 0][c(1,3),])[,criterion := "outcomes & difficulty"]
  s = rbind(best,inequaloutcomes)
  write.table(s, "../stimuli/stimuli-trial3.csv",sep=";", dec=".", row.names=F)
  plot = make_plot_critical(merge(sim[samplinggroup == "3"],unique(s[, id := as.character(id)]),by = c("id","trial","state")))
  ggsave("../figures/stimuli-t3.png", width = 10, height = 7, scale = 1)
}



