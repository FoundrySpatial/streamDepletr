hunt <- function(t, d, S, Tr, lmda){
  #' Hunt (1999) analytical model for streamflow depletion with a partially-penetrating stream.
  #' @param t times you want output for [T]
  #' @param d distance from well to stream [L]
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param lmda streambed conductance term, lambda [L/T]
  #' 
  #' Reference:
  #' Hunt, B (1999). Unsteady Stream Depletion from Ground Water Pumping. 
  #' Ground Water 37 (1): 98-102. doi:10.1111/j.1745-6584.1999.tb00962.x.
  #' 
  #' See script: Hunt1999_lmda to estimate lmda based on aquifer and well properties
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
  
  # solve for Qf
  Qf <- (erfc(sqrt((S*d*d)/(4*Tr*t))) - 
           exp((lmda*lmda*t)/(4*S*Tr) + (lmda*d)/(2*Tr))*
           erfc(sqrt((lmda*lmda*t)/(4*S*Tr))+sqrt((S*d*d)/(4*Tr*t))))
  
  # if solver is NOT stable: print a warning, adjust lmda downwards, and re-solve
  if (sum(!is.finite(Qf)) != 0){
    warning("Unstable combination of input parameters; revising lmda downwards to ensure solution.")
  } 
  
  while (sum(!is.finite(Qf)) != 0){
    lmda <- lmda*0.99
    Qf <- (erfc(sqrt((S*d*d)/(4*Tr*t))) - 
             exp((lmda*lmda*t)/(4*S*Tr) + (lmda*d)/(2*Tr))*
             erfc(sqrt((lmda*lmda*t)/(4*S*Tr))+sqrt((S*d*d)/(4*Tr*t))))
  }
  
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
# # define z options (different lines)
# z <- c(1, 0.3, 0.1, 0.03, 0.01)
# 
# # set constant lambda, t, and S
# lmda <- rep(1, length(x))
# t <- rep(1, length(x))
# S <- rep(1, length(x))
# 
# rm(df.all)
# for (z.in in z){
#   # calculate Tr and d as function of input parameters
#   Tr <- 4/(x*z.in*z.in)
#   d <- Tr*z.in
# 
#   # make data frame with Qf
#   df <- data.frame(x=x,
#                    z=z.in,
#                    Qf=hunt(t=t, d=d, S=S, Tr=Tr, lmda=lmda))
# 
#   # add to data frame with all z
#   if (exists("df.all")){
#     df.all <- rbind(df.all, df)
#   } else {
#     df.all <- df
#   }
# }
# 
# # make plot
# require(ggplot2)
# p <-
#   ggplot(df.all, aes(x=x, y=Qf, color=factor(z))) +
#   geom_line() +
#   scale_x_log10(name="(4*Tr*t)/(S*d*d)", expand=c(0,0), breaks=lseq(0.1,10000,6)) +
#   scale_y_continuous(name="Streamflow Depletion Fraction, Qf", limits=c(0,1), expand=c(0,0)) +
#   scale_color_discrete(name="lmda*d/Tr") +
#   theme_bw() +
#   theme(panel.grid=element_blank())
# p