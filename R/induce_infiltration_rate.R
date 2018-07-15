induce_infiltration_rate <- function(d, Qa){
  #' Calculate pumping rate at which pumping will induce infiltration from stream.
  #'
  #' This is the critical pumping rate above which induced infiltration due to groundwater pumping will occur,
  #' based on the \link{glover} model of streamflow depletion. Derived in Wilson (1993) Eq. 5.
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
  #' @param Qa ambient groundwater inflow rate per unit length of stream [L2/T]
  #' @return A numeric of \code{Qc}, critical pumping rate above which induced infiltration due to groundwater pumping will occur [L3/T].
  #' @references
  #' Wilson, JL (1993). Induced Infiltration in Aquifers with Ambient Flow. Water Resources Research 
  #' 29(10): 3503-12. doi:10.1029/93WR01393.
  #' @examples
  #' induce_infiltration_rate(d=100, Qa=10)
  #' induce_infiltration_rate(d=100, Qa=50)
  #' induce_infiltration_rate(d=500, Qa=50)
  #' @export
  
  Qc <- pi*d*Qa
  return(Qc)
  
}
