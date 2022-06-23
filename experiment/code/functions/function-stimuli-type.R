#===============================================================
# function for the stimuli type
#
#===============================================================

# re = rare_event (should choice problem contain a rare event; re vs. nore)
# the trial, we are interested in
# direction: does the optimal model predict risky or safe
# equaloutcomes: should choice problems with values in the outcomes for risky and safe be excluded (no = no equaloutcomes)
# equalprobabilities: should choice problems with equal probabilities for risky and safe be excluded (no = no equalprobabilities)

type = function(data, re = NULL, t, direction = NULL, equaloutcomes = NULL, equalprobabilities = NULL){
  
  # filter
  if(!missing(t)){
    data = data[trial %in% t]
  }
  
  
  if(re == "re"){ 
    data = data[rare_event == 1]
  } else if(re == "nore") {
    data = data[rare_event == 0]
  } else {
    data = data
  }
  
  if(direction == "risky"){
    data = data[direction == 1]
  } else if (direction == "safe") {
    data = data[direction == -1]
  } else {
    data = data
  }
  
  if(equaloutcomes == "no"){
    data = data[equal_outcomes == 0]
  }
  
  if(equalprobabilities == "no"){
    data = data[equal_probabilities == 0]
  }
  
  return(data)
}