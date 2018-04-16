Wilson1993_BetweenStreams_Unconfined <- function(x, y, K, Qw, h.1, h.2, L){
  ## Wilson (1993) analytical model for head distribution with ambient groundwater flow
  ## for a well between two streams.
  #'
  #' Reference:
  #' Wilson, JL (1993). Induced infiltration in aquifers with ambient flow. Water Resources Research 29(10): 3503-3512. doi:10.1029/93WR01393
  #'   Equation 2
  #'
  #' Inputs:
  #'  x  = x-coordinate of interest [L]
  #'  y  = y-coordinate of interest [L]
  #'  K  = horizontal hydraulic conductivity of aquifer [L/T]
  #'  Qw = pumping rate of well [L3/T]
  #'  N  = vertical recharge rate [L/T]
  #'  d  = distance from well to stream [L]
  #'  L  = distance between streams [L]
  #'  h.1= head of stream 1 (left side) [L], at position y=0
  #'  h.2= head of stream 2 (right side) [L], at position y=L
  #'  
  #'  Well is at coordinate (x=0, y=d)
  #'  
  #' Output:
  #'  h  = head at coordinate (x,y) [L]
  #'  
  #' Assumptions:
  #'  -Fully penetrating well screen
  #'  -Fully penetrating stream
  #'  -Dupuit-Forchheimer assumptions
  #'  -Constant head in stream
  #'  -Steady state
  #'  -Parallel streams
  #'  
  #' Example run code is at bottom of this .R file (below function)
  
  P.1 <- (K*h.1*h.1)/2  # psi at stream 1
  P.2 <- (K*h.2*h.2)/2  # psi at stream 2
  P <- ...
  h <- sqrt(P*2/K)
  return(h)
}

# ## example runscript
# x   <- 0             # cross-section
# y   <- seq(1,1000)   # 1000 m
# K   <- 0.1           # 0.1 m/d
# h.0 <- 100           # head of 100 m at stream
# qa  <- 0.002*K*100   # gradient of 2 m/1000 m, 50 m thick aquifer
# d   <- 500.5         # well position [m]
# Qw  <- 38            # 38 m3/d is about 10,000 gallons/day
# 
# h <- Wilson1993_SemiInfinite_Unconfined(x, y, K, h.0, qa, d, Qw)
# plot(y,h)
