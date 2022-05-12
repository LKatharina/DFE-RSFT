#==========================================================================
# Calculate utility of each choice problem and plot choice problem with
# the highest utility
#==========================================================================

# Packages ----------------------------------------------------------------
pacman::p_load(data.table, ggplot2, tidybayes)

# load data ---------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions/function-utility.R")
source("functions/function-make-plot.R")

sim <- readRDS("../stimuli/list-stimuli-t3-predictions-101-200.rds")
sim <- as.data.table(sim)

# Parameters
w1 <- w2 <- w3 <- 1/3

# Utility
uoftask = utility(sim,w1,w2,w3)
idoftask = uoftask[uoftask[, utility == max(utility)]]$id

# Plot 1
make_plot(sim[id == idoftask])
#make_plot(sim[id == 2])

# Plot 2
densplot(sim,sim[id == idoftask])
sim[id == idoftask & trial == 3 & state == 18]
