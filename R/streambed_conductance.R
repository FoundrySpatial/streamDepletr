streambed_conductance <- function(w, Kriv, briv){
  #'Estimate streambed conductance.
  #'
  #' @param w stream width [L]
  #' @param Kriv streambed semipervious layer hydraulic conductivity [L/T]. 
  #'          Reeves et al. (2009) estimate this as the vertical hydraulic 
  #'          conductivity of the aquifer (\code{Kv}; L/T), which is itself often estimated
  #'          as 10\% of the horizontal hydraulic conductivity (\code{Kh*0.1}; L/T)
  #' @param briv streambed semipervious layer thickness [L]
  #'          Reeves et al. (2009) estimate this as the vertical distance from
  #'          the streambed to the top of the well screen, or the length of the
  #'          well screen, whichever is greater [L].
  #' @return `lmda`` streambed conductance term, lambda [L/T]
  
  lmda <- w*Kriv/briv
  return(lmda)
}