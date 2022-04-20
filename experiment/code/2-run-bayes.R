#==========================================================================
# Run bayes.R
#
#==========================================================================

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)

# Load data and Functions --------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../analyses/models/rsft1988.R")
source("../../analyses/models/softmax.R")
source("functions/function-bayes.R")
source("functions/function-set-bayes.R")
d = fread("../stimuli/list-stimuli-t3.csv")


# Set Parameters ------------------------------------------------------------
ss = c(3,10)
parallel = T

# Dataset -------------------------------------------------------------------
d = merge(d,d[,ss, by = id], by=c("id"))
d[, id := as.character(id)]
d[, samplinggroup := as.character(ss)]


# Simulate decisions from experience according to the cognitive bayesian learning model
if (parallel) {
  library(doMC)
  library(parallel)
  cores <- detectCores()
  d[, core := rep(1:cores, length.out = nrow(d))] 
  registerDoMC(cores = detect.Cores())
  sim <- foreach(x = 1:cores, 
                   .export = c("computeBeliefs", "rsftModel", "cr_softmax", "step", "data.table","cognitivemodels", "bayesbeliefs", "cores"), 
                   .combine = "rbind") %dopar% {
                     d[core == x, bayesbeliefs(.SD), by = c("samplinggroup","id")]
                   }
  } else {
    sim <- d[, bayesbeliefs(.SD), by = c("samplinggroup","id")]
}

sim = merge(sim,d[,core := NULL],by = c("id","samplinggroup"))
saveRDS(sim,"../stimuli/list-stimuli-t3-predictions.rds")
