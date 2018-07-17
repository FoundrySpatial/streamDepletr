induce_infiltration_time <- function(d, S, Tr, Qa, Qw) {
  #' Calculate critical time at which stream transitions from gaining to losing.
  #'
  #' This is the time at which induced infiltration due to groundwater pumping begins,
  #' based on the \link{glover} model of streamflow depletion. Derived in Chen (2003) Eq. 4.
  #'
  #' Assumptions:
  #' \itemize{
  #'   \item Groundwater flow is perpendicular to stream
  #'   \item Horizontal flow >> vertical flow (Dupuit assumptions hold)
  #'   \item Homogeneous, isotropic aquifer
  #'   \item Constant \code{Tr}: Aquifer is confined, or if unconfined change in head is small relative to aquifer thickness
  #'   \item Stream is straight, infinitely long, and remains in hydraulic connection to aquifer
  #'   \item Constant stream stage
  #'   \item No changes in recharge due to pumping
  #'   \item No streambank storage
  #'   \item Constant pumping rate
  #'   \item Aquifer extends to infinity
  #'   \item Stream fully penetrates through aquifer
  #'   \item No streambed resistance to flow (see \link{hunt} or \link{hantush} for streambed resistance)
  #' }
  #'
  #' @param d distance from well to stream [L]
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param Qa ambient groundwater inflow rate per unit length of stream [L2/T]
  #' @param Qw well pumping rate [L3/T]
  #' @return A numeric of \code{tc}, the critical time at which induced infiltration begins [T].
  #' @references
  #' Chen, X (2003). Analysis of Pumping-Induced Stream-Aquifer Interactions for Gaining Streams.
  #' Journal of Hydrology 275(1): 1-11. doi:10.1016/S0022-1694(02)00320-7
  #' @examples
  #' # recreate Figure 2 in Chen (2003)
  #' Qa <- c(0.0001, 0.0003, 0.0005, 0.0008, 0.001)
  #' tc <- induce_infiltration_time(d=575, S=0.2, Tr=100*15, Qa=Qa, Qw=2727)
  #' plot(x=(pi*Qa*100*15*575/2727), y=tc, log="y")
  #' @export

  tc <- -d * d * S / (4 * Tr * log(pi * Qa * Tr * d / Qw))
  return(tc)
}
