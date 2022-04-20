#===============================================================
# Create dataset containing stimuli and RSFT Predictions
#
#===============================================================

# Load Packages-------------------------------------------------
pacman::p_load(data.table)


# Source -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../analyses/models/rsft1988.R")
source("functions/function-createstimuli.R")
source("functions/function-preselect-stimuli.R")


# Parameters -----------------------------------------------------
ntrials = 3

# Create Stimuli -------------------------------------------------
data = createStimuli(
  possibleoutcomes = 1:10,
  possibleprobabilities = c(.1 , .2, .25, .3, .4, .5, .6, .7, .75, .8, .9),
  ntrials = ntrials,
  start = 0,
  mindiff_var = 5
  )

# Preselect --------------------------------------------------------
data = preselect(
  data,
  maxdifficulty = 0.91,
  mindifficulty = 0.1,
  prbadrows = 0.5
  )

# Save -------------------------------------------------------------
fwrite(data,"../stimuli/list-stimuli-t3.csv")




