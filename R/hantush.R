hantush <- function(t, d, S, Kh, b, Kriv, briv, prec=80){
  #' Streamflow depletion in partially penetrating stream with semipervious streambed. 
  #' 
  #' Described in Hantush (1965). As the leakance term \code{(b*Kh/Kriv)} approaches 0 this is equivalent to \code{glover}.
  #' 
  #' Assumptions:
  #' \itemize{
  #'   \item Horizontal flow >> vertical flow (Dupuit assumptions hold)
  #'   \item Homogeneous, isotropic aquifer
  #'   \item Constant \code{Tr}: Aquifer is confined, or if unconfined change in head is small relative to aquifer thickness
  #'   \item Stream is straight, infinitely long, and remains in hydraulic connection to aquifer
  #'   \item Constant stream stage
  #'   \item No changes in recharge due to pumping
  #'   \item No streambank storage
  #'   \item Constant pumping rate
  #'   \item Aquifer extends to infinity
  #' }
  #' 
  #' @param t times you want output for [T]
  #' @param d distance from well to stream [L]
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Kh aquifer horizontal hydraulic conductivity [L/T]
  #' @param b aquifer saturated thickness [L]
  #' @param Kriv streambed semipervious layer hydraulic conductivity [L/T]
  #' @param briv streambed semipervious layer thickness [L]
  #' @param prec precision for mpfr package for storing huge numbers; 80 seems to generally work but tweak this if you get weird results.
  #' @return A numeric of \code{Qf}, streamflow depletion as fraction of pumping rate [-]. 
  #' If the pumping rate of the well (\code{Qw}; [L3/T]) is known, you can calculate volumetric streamflow depletion [L3/T] as \code{Qf*Qw}
  #' @references
  #' Hantush, MS (1965). Wells near Streams with Semipervious Beds. Journal of Geophysical Research 70(12): 2829-38. doi:10.1029/JZ070i012p02829.
  #' @export

  # streambed leakance
  L <- (Kh/Kriv)*briv
  
  # transmissivity
  Tr <- Kh*b
  
  # erfc and exp terms can get really huge; use the mpfr package to deal with them
  term1 <- Rmpfr::mpfr(sqrt(S*d*d/(4*Tr*t)), prec)
  term2 <- Rmpfr::mpfr((((Tr*t)/(S*L*L))+(d/L)), prec)
  term3 <- Rmpfr::mpfr((sqrt((Tr*t)/(S*L*L))+sqrt((S*d*d)/(4*Tr*t))), prec)
  
  # calculate streamflow depletoin
  Qf <- as.numeric(
    Rmpfr::erfc(term1) - exp(term2)*Rmpfr::erfc(term3)
  )
  
  return(Qf)
}

# ## Example: Rathfelder (2016) Figure 42
# require(ggplot2)
# t    <- seq(1,720) # [d]
# Kh   <- 10         # [m/d]
# S    <- 0.25
# d    <- 30         # [m]
# briv <- 1          # [m]
# b    <- 20         # [m]
# 
# hantush(t=89*86400, d=d, S=S, Kh=Kh/86400, b=b, Kriv=1/86400, briv=briv)
# 
# df <- data.frame(times=numeric(0), Qf=numeric(0), Kriv=numeric(0))
# for (Kriv in c(1, 0.1, 0.01)){
#   df <- rbind(df,
#               data.frame(times = t,
#                          Qf = hantush(t=t, d=d, S=S, Kh=Kh, b=b, Kriv=Kriv, briv=briv),
#                          Kriv = Kriv))
# }
# 
# ggplot(df, aes(x=times, y=Qf, color=factor(Kriv))) + geom_line()
#
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
# Kh <- 1
# b <- 1
# briv <- 1
# Tr <- Kh*b
# 
# # calculate d and K.sb based on x-axis
# d <- sqrt((4*Tr*t)/(x*S))
# Kriv <- Kh/(d/2)
# 
# # solve analytical solution
# df.all <- data.frame(x = x,
#                      Qf = hantush(t=t, d=d, S=S, Kh=Kh, b=b, Kriv=Kriv, briv=briv))
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
