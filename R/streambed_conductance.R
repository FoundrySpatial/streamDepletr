streambed_conductance <- function(w, Kriv, briv){
  ## Script to estimate lambda (streambed conductance) parameter for 
  #' Hunt (1999) streamflow depletion model.
  #' 
  #' Inputs:
  #'  w     = stream width [L]
  #'  Kriv  = riverbed hydraulic conductivity [L/T]
  #'          Reeves et al. (2009) estimate this as the vertical hydraulic 
  #'          conductivity of the aquifer (Kv; L/T), which is itself often estimated
  #'          as 10% of the horizontal hydraulic conductivity (Kh*0.1; L/T)
  #'  briv  = thickness of riverbed semipervious layer [L]
  #'          Reeves et al. (2009) estimate this as the vertical distance from
  #'          the streambed to the top of the well screen, or the length of the
  #'          well screen, whichever is greater [L].
  #'  
  #' Output:
  #'  lmda = streambed conductance term, lambda [L/T]
  
  lmda <- w*Kriv/briv
  return(lmda)
}