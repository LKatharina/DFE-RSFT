# ===================================================================================
# Add attention check stimuli: make final stimuli file
# 
# ===================================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stimuli <- fread("../stimuli/Stimuli_allinfos.csv")
stimuli = stimuli[,.(xh,yh,pxh,pyh,xl,yl,pxl,pyl,b,start,state,trial,type)]

# Attention check stimuli
a1 = data.table(2,1,0.1,0.9,5,6,0.2,0.8,15,0,10,3,"attention")
a2 = data.table(3,4,0.8,0.2,10,12,0.7,0.3,14,0,4,2,"attention")

# Colnames 
cnames = c("x1HV", "x2HV", "p1HV", "p2HV", "x1LV", "x2LV", "p1LV", "p2LV", "budget", "start","state","trial","type")
colnames(a1) = cnames
colnames(a2) = cnames
colnames(stimuli) = cnames
stimuli = rbind(stimuli,a1,a2)
fwrite(stimuli, "../stimuli/stimuli.csv")
stimuli[,type := NULL]
fwrite(stimuli, "../rsft-dfe-experiment/rsft_dfe_experiment/static/stimuli/stimuli.csv")