NewsomWilson1988 <- function(d, Qw, qa, alpha){
  ## Newsom & Wilson (1988) analytical model for streamflow depletion ambient groundwater flow.
  #'
  #' Reference:
  #' Newsom, JM, & JL Wilson (1988). Flow of Ground Water to a Well Near a Stream - Effect of Ambient Ground-Water Flow Direction. Ground Water 26(6): 703-711. doi:10.1111/j.1745-6584.1988.tb00420.x
  #'
  #' Inputs:
  #'  d  = distance from well to stream [L]
  #'  Qw = pumping rate of well [L3/T]
  #'  qa = ambient groundwater flow rate towards stream per unit length of stream [L2/T]
  #'  alpha = direction of ambient groundwater flow [deg] - 0 = perpendicular to stream, 90 = parallel to stream
  #'  
  #' Output:
  #'   Bv = critical dimensionless pumping rate at which induced infiltration occurs
  #'        
  #'  
  #' Example run code is at bottom of this .R file (below function)
  
  # load package needed for erfc
  require(pracma)
  
  # dimensionless pumping rate
  beta <- Qw/(qa*pi*d)
  
  if (x>=d) {
    c <- pi
  } else {
    c <- 0
  }
  
  # location of stagnation point (positive coordinate; will also be negative image coordinate)
  x.stag <- ((d*d/2)*((1-beta*cosd(alpha))+((1+beta*beta-2*beta*cosd(alpha))^0.5)))^0.5
  y.stag <- ((d*d/2)*((beta*cosd(alpha)-1)+((1+beta*beta-2*beta*cosd(alpha))^0.5)))^0.5
  
  return(Qf)
}

# code for testing
alpha <- 90