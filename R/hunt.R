hunt <- function(t, d, S, Tr, lmda, prec=80){
  #'Streamflow depletion in partially penetrating stream with semipervious streambed. 
  #'
  #' Described in Hunt (1999). When \code{lmda} term gets very large, this is equivalent to \code{glover}.
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
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param lmda streambed conductance term, lambda [L/T]. Can be estimated with \code{streambed_conductance}.
  #' @param prec precision for \code{Rmpfr} package for storing huge numbers; 80 seems to generally work but tweak this if you get weird results. Reducing this value will reduce accuracy but speed up computation time.
  #' @return \code{Qf}, numeric or vector of streamflow depletion as fraction of pumping rate [-]. 
  #' If the pumping rate of the well (\code{Qw}; [L3/T]) is known, you can calculate volumetric streamflow depletion [L3/T] as \code{Qf*Qw}
  #' @references
  #' Hunt, B (1999). Unsteady Stream Depletion from Ground Water Pumping. 
  #' Ground Water 37 (1): 98-102. doi:10.1111/j.1745-6584.1999.tb00962.x.
  #' @export
  
  # erfc and exp terms can get really huge; use the Rmpfr package to deal with them
  term1 <- Rmpfr::mpfr(sqrt((S*d*d)/(4*Tr*t)), prec)
  term2 <- Rmpfr::mpfr(((lmda*lmda*t)/(4*S*Tr) + (lmda*d)/(2*Tr)), prec)
  term3 <- Rmpfr::mpfr(sqrt((lmda*lmda*t)/(4*S*Tr))+sqrt((S*d*d)/(4*Tr*t)), prec)
  
  Qf <- as.numeric(
    Rmpfr::erfc(term1) - exp(term2)*Rmpfr::erfc(term3)
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
