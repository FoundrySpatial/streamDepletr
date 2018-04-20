hantush <- function(d, S, Tr, t, Kf.sb, B.sb){
  ## Hantush (1965) analytical model for streamflow depletion with partially penetrating stream with semipervious streambed.
  #'
  #' Reference:
  #' Hantush, MS (1965). Wells near Streams with Semipervious Beds. Journal of Geophysical Research 70(12): 2829-38. doi:10.1029/JZ070i012p02829.
  #'
  #' Inputs:
  #'  d  = distance from well to stream [L]
  #'  S  = aquifer storage coefficient (storativity) [-]
  #'  Tr = aquifer transmissivity [L2/T]
  #'  t  = time since pumping started [T]
  #'  Kf.sb = ratio of (aquifer hydraulic conductivity) / (streambed hydraulic conductivity) [-]
  #'  B.sb  = thickness of streambed semipervious layer [L]
  #'  
  #' Output:
  #'   Qf = streamflow depletion as fraction of pumping rate [-]
  #'   
  #' If you have the pumping rate of the well [Qw; L3/T] you can
  #' calculate the rate of streamflow depletion [Qs; L3/T] as Qs=Qf*Qw
  #'   
  #' Assumptions (from Reeves et al., 2009):
  #'  -Horizontal flow >> vertical flow (Dupuit assumptions hold)
  #'  -Homogeneous, isotropic aquifer
  #'  -Aquifer is confined, or if unconfined change in head is small relative to total thickness (constant Tr)
  #'  -Stream is straight, infinitely long, and remains in hydraulic connection to aquifer
  #'  -Pumping does not change the stage of the stream  
  #'  -Recharge to the system is unchanged by pumping
  #'  -Streambed may offer resistance to groundwater flow
  #'  -No streambank storage
  #'  -Pumping rate is constant
  #'  -Aquifer extends to infinity
  #'  
  #' Example run code is at bottom of this .R file (below function)
  
  # load package needed for erfc
  require(pracma)
  
  L <- Kf.sb*B.sb
  Qf <- erfc(sqrt(S*d*d/(4*Tr*t))) - 
    exp(((Tr*t)/(S*L*L))+(d/L))*
    erfc((sqrt((Tr*t)/(S*L*L))+sqrt((S*d*d)/(4*Tr*t))))
  return(Qf)
}

# ## Example script to reproduce Figure 4 from Hunt (1999)
# lseq <- function(from=1, to=100000, length.out=6) {
#   # logarithmic spaced sequence
#   # blatantly stolen from library("emdbook"), because need only this
#   exp(seq(log(from), log(to), length.out = length.out))
# }
# 
# # define range for x=(4*Tr*t)/(S*d*d)
# x <- lseq(0.1, 10000, length.out=100)
# 
# # set constant t, S, Tr, B.sb
# t <- 1
# S <- 1
# Tr <- 1
# B.sb <- 1
# 
# # calculate d and K.sb based on x-axis
# d <- sqrt((4*Tr*t)/(x*S))
# Kf.sb <- d/2
# 
# # solve analytical solution
# df.all <- data.frame(x = x,
#                      Qf = hantush(d=d, S=S, Tr=Tr, t=t, Kf.sb=Kf.sb, B.sb=B.sb))
# 
# ## some NaNs appear towards the upper end here- not sure what's happening
# 
# # make plot
# require(ggplot2)
# p <-
#   ggplot(df.all, aes(x=x, y=Qf)) +
#   geom_line() +
#   scale_x_log10(name="(4*Tr*t)/(S*d*d)", expand=c(0,0), breaks=lseq(0.1,10000,6)) +
#   scale_y_continuous(name="Streamflow Depletion Fraction, Qf", limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(panel.grid=element_blank())
# p