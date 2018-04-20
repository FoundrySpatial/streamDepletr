Hunt1999_lmda <- function(Kv, w, beff){
  ## Script to estimate lambda parameter for Hunt1999.R streamflow depletion model.
  #' Equation 3 in Reeves et al. (2009)
  #' 
  #' Inputs:
  #'  Kv = vertical hydraulic conductivity of aquifer [L/T]
  #'  w  = stream width [L]
  #'  beff  = distance from bottom of stream to top of well screen [L]
  #'  
  #' Output:
  #'  lmda = streambed conductance term, lambda [L/T]
  
  lmda <- Kv*w/beff
  return(lmda)
}