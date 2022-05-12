#==========================================================================
# Run bayes.R
#
#==========================================================================

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork,future, doFuture, doRNG)
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
ss = c(3) # c(3, 10)
parallel = T

# Dataset -------------------------------------------------------------------
d = merge(d,d[,ss, by = id], by=c("id"))
d <- d[id %in% 1:500] # change this!!
d[, id := as.character(id)]
d[, samplinggroup := as.character(ss)]

# Simulate decisions from experience according to the cognitive bayesian learning model
if (parallel) {
  library(parallel)
  cores <- availableCores()
  d[, core := rep(1:cores, each = round(nrow(d)/length(cores),0) ,length.out = nrow(d))]
  registerDoFuture()
  plan(multisession)
  options(future.globals.onReference = NULL)
  system.time({
  sim <- foreach(x = 1:cores,
                  .export = c("computeBeliefs", "rsftModel", "cr_softmax", "step", "data.table","cognitivemodels", "bayesbeliefs", "cores", "RSFT"), 
                   .combine = "rbind")  %dorng%  {
                     setClass(Class="RSFT",
                              representation(
                                compact = "data.table",
                                extended = "data.table"
                              )
                     )
                     d[core == x, bayesbeliefs(.SD), by = c("samplinggroup","id")]
                   }
  })
  sim = merge(sim,d[,core := NULL],by = c("id","samplinggroup"))
  } else {
    system.time({
    sim <- d[, bayesbeliefs(.SD), by = c("samplinggroup","id")]
    })
    sim = merge(sim,d,by = c("id","samplinggroup"))
}


saveRDS(sim, paste0("../stimuli/list-stimuli-t3-predictions",min(d$id),".rds"))
