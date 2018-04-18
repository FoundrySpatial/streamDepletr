Hunt1999 <- function(d, S, Tr, t, lmda, lmda_max){
  ## Hunt (1999) analytical model for streamflow depletion with a partially-penetrating stream.
  #'
  #' Reference:
  #' Hunt, B (1999). Unsteady Stream Depletion from Ground Water Pumping. Ground Water 37 (1): 98-102. doi:10.1111/j.1745-6584.1999.tb00962.x.
  #' 
  #' Inputs:
  #'  d  = distance from well to stream [L]
  #'  S  = aquifer storage coefficient [-] (specific yield for unconfined storativity for confined)
  #'  Tr = aquifer transmissivity [L2/T]
  #'  t  = time since pumping started [T]
  #'  lmda = streambed conductance term, lambda [L/T]
  #'    See script: Hunt1999_lmda to estimate this based on aquifer and well properties
  #'  lmda_max = maximum allowed value of lmda; if lmda>lmda_max, lmda will be set = lmda_max
  #'    This is necessary because in some settings lmda can get super high leading to erfc and exp terms
  #'    with exponents outside R's ability to solve them. Huggins et al. (2018) JAWRA set lmda_max=1, 
  #'    but it is not clear what units they are using and this has a significant impact on the results
  #'    for trying to reproduce Rathfelder (2016) Fig. 61.
  #'    lmda (and lmda_max) are important over short timescales but don't have a big impact on the asymptote.
  #'  
  #' Output:
  #'   Qf = streamflow depletion as fraction of pumping rate [-]
  #'   
  #' If you have the pumping rate of the well [Qw; L3/T] you can
  #' calculate the rate of streamflow depletion [Qs; L3/T] as Qs=Qf*Qw
  #'   
  #' Assumptions (from Reeves et al., 1999):
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
  
  # make sure lmda is not > lmda_max
  lmda <- min(c(lmda, lmda_max))
  
  # solve for Qf
  Qf <- (erfc(sqrt((S*d*d)/(4*Tr*t))) - 
           exp((lmda*lmda*t)/(4*S*Tr) + (lmda*d)/(2*Tr))*
           erfc(sqrt((lmda*lmda*t)/(4*S*Tr))+sqrt((S*d*d)/(4*Tr*t)))
         )
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
#                    Qf=Hunt1999(d=d, S=S, Tr=Tr, t=t, lmda=lmda))
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