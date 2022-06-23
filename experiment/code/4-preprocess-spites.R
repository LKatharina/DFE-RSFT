# ===================================================================================
# Process Stimuli to create Sprites 
# 
# ===================================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stimuli <- fread("../stimuli/Stimuli_allinfos.csv")

stimuli = stimuli[,.(xh,yh,pxh,pyh,xl,yl,pxl,pyl,b,start,state,trial,type)]

colnames(stimuli) = c("x1HV", "x2HV", "p1HV", "p2HV", "x1LV", "x2LV", "p1LV", "p2LV", "budget", "start","state","trial","type")

fwrite(stimuli, "../stimuli/Stimuli.csv")
