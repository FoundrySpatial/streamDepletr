GloverBalmer1954 <- function(d, S, Tr, t){
  ## Glover and Balmer (1954) analytical model for streamflow depletion with fully-penetrating stream.
  #'
  #' Reference:
  #' Glover, RE, and GG Balmer (1954)."River Depletion Resulting from Pumping a Well near a River. Eos, Transactions American Geophysical Union 35(3): 468-70. doi:10.1029/TR035i003p00468.
  #'   
  #' Inputs:
  #'  d  = distance from well to stream [L]
  #'  S  = aquifer storage coefficient (storativity) [-]
  #'  Tr = aquifer transmissivity [L2/T]
  #'  t  = time since pumping started [T]
  #'  
  #' Output:
  #'   Qf = streamflow depletion as fraction of pumping rate [-]
  #'   
  #' If you have the pumping rate of the well [Qw; L3/T] you can
  #' calculate the rate of streamflow depletion [Qs; L3/T] as Qs=Qf*Qw
  #' 
  #' Assumptions (from Hunt, 1999):
  #'  -Horizontal flow >> vertical flow (Dupuit assumptions hold)
  #'  -Homogeneous, isotropic aquifer
  #'  -Aquifer is confined, or if unconfined change in head is small relative to total thickness (constant Tr)
  #'  -Stream is straight, infinitely long, and remains in hydraulic connection to aquifer
  #'  -Pumping does not change the stage of the stream  
  #'  -Recharge to the system is unchanged by pumping
  #'  -No streambank storage
  #'  -Pumping rate is constant
  #'  -Aquifer extends to infinity
  #'  
  #' Example run code is at bottom of this .R file (below function)

  # load package needed for erfc
  require(pracma)
  
  Qf <- erfc(sqrt(S*d*d/(4*Tr*t)))
  return(Qf)
}

# ## Example script to reproduce Table 1 from Glover & Balmer (1954)
# GloverBalmer1954(d=1000,  S=0.2, Tr=0.1, t=157770000)  # well 1
# GloverBalmer1954(d=5000,  S=0.2, Tr=0.1, t=157770000)  # well 2
# GloverBalmer1954(d=10000, S=0.2, Tr=0.1, t=157770000)  # well 3
